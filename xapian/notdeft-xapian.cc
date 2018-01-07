#include <algorithm>
#include <ctype.h>
#include <dirent.h>
#include <fstream>
#include <iostream>
#include <string.h>
#include <sys/stat.h>
#include <tclap/CmdLine.h>
#include <unistd.h>
#include <xapian.h>

#define __STDC_FORMAT_MACROS
#include <inttypes.h>

using namespace std;

/** Serializes in a sorting friendly way, similarly to
    `Xapian::sortable_serialise`. Should be quite portable when the
    argument is coerced from `time_t`, although C does not actually
    even guarantee an integer type in that case. */
string time_serialize(const int64_t v) {
  char buf[16+1];
  // format in hexadecimal, zero padded, 64/4 digits
  if (snprintf(buf, sizeof buf, "%016" PRIx64, v) != 16) {
    // POSIX requires `errno` to be set, but C does not
    throw Xapian::AssertionError("unexpected snprintf failure", errno);
  }
  return string(buf);
}

/** The inverse of `time_serialize`. */
int64_t time_deserialize(const string& s) {
  int64_t v;
  if (sscanf(s.c_str(), "%" SCNx64, &v) != 1) {
    throw Xapian::InvalidArgumentError("bad time_deserialize arg", errno);
  }
  return v;
}

bool string_starts_with(const string& s, const string& pfx) {
  return s.compare(0, pfx.length(), pfx) == 0;
}

bool string_ends_with(const string& s, const string& sfx) {
  const int pos = s.length() - sfx.length();
  return (pos >= 0) && (s.compare(pos, sfx.length(), sfx) == 0);
}

bool string_ends_with_one_of(const string& s, const vector<string>& sfxs) {
  for (const string& sfx : sfxs) {
    if (string_ends_with(s, sfx)) {
      return true;
    }
  }
  return false;
}

bool whitespace_p(const string& s) {
  for (auto p = s.c_str(); *p; p++)
    if (!isspace(*p))
      return false;
  return true;
}

string downcase(const string& s) {
  string data;
  data.resize(s.length());
  std::transform(s.begin(), s.end(), data.begin(), ::tolower);
  return data;
}

bool file_directory_p(const string& file) {
  struct stat sb;
  return (stat(file.c_str(), &sb) == 0) && S_ISDIR(sb.st_mode);
}

/** Returns an empty list on failure. */
vector<string> ls(const string& file) {
  vector<string> lst;
  DIR* dir = opendir(file.c_str());
  if (dir == NULL)
    return lst;
  struct dirent* entry;
  while ((entry = readdir(dir)) != NULL) {
    string name(entry->d_name);
    if (!(string_starts_with(name, ".") ||
	  string_starts_with(name, "_") ||
	  string_starts_with(name, "#"))) {
      lst.push_back(name);
    }
  }
  closedir(dir);
  return lst;
}

string file_join(const string& x, const string& y) {
  if (x == ".")
    return y;
  if (string_ends_with(x, "/"))
    return x + y;
  return x + "/" + y;
}
  
string file_non_directory(const string& s) {
  auto found = s.find_last_of('/');
  if (found == string::npos)
    return s;
  return string(s.substr(found + 1));
}

string file_basename(const string& s) {
  auto basename = file_non_directory(s);
  auto found = basename.find_last_of('.');
  if (found == string::npos)
    return basename;
  return string(basename.substr(0, found));
}

string file_extension(const string& s) {
  auto basename = file_non_directory(s);
  auto found = basename.find_last_of('.');
  if (found == string::npos)
    return "";
  return string(basename.substr(found));
}

void ls_org(vector<string>& res, const string& root,
	    const string& dir, const vector<string>& exts) {
  auto absDir = file_join(root, dir);
  for (const string& file : ls(absDir)) {
    auto relFile = file_join(dir, file);
    auto absFile = file_join(absDir, file);
    if (string_ends_with_one_of(file, exts)) {
      res.push_back(relFile);
    } else if (file_directory_p(absFile)) {
      ls_org(res, root, relFile, exts);
    }
  }
}

static void usage()
{
  cerr << "notdeft-xapian" << endl;
  cerr << "USAGE:" << endl;
  cerr << "To build/refresh search indices" << endl;
  cerr << "(for specified directories):" << endl;
  cerr << "  notdeft-xapian index [options] directory..." << endl;
  cerr << "To find text documents" << endl;
  cerr << "(matching the specified query):" << endl;
  cerr << "  notdeft-xapian search [options] directory..." << endl;
}

static constexpr Xapian::valueno DOC_MTIME = 0;

static int doIndex(vector<string> subArgs) {
  TCLAP::CmdLine cmdLine
    ("Specify the directories to index."
     " Any relative paths are stored as given."
     " Search results are reported in the same manner,"
     " regardless of the search time working directory.");
  TCLAP::ValueArg<string>
    langArg("l", "lang", "stemming language (e.g., 'en' or 'fi')",
	    false, "en", "language");
  cmdLine.add(langArg);
  TCLAP::MultiArg<string>
    extArg("x", "extension", "filename extension (default: '.org')",
	    false, "extension");
  cmdLine.add(extArg);
  TCLAP::ValueArg<string>
    chdirArg("c", "chdir", "change working directory first",
	    false, ".", "directory");
  cmdLine.add(chdirArg);
  TCLAP::SwitchArg
    resetArg("r", "recreate", "recreate database", false);
  cmdLine.add(resetArg);
  TCLAP::ValueArg<int>
    titleArg("t", "title-wdf", "title importance (default: 10)",
	    false, 10, "wdf_inc");
  cmdLine.add(titleArg);
  TCLAP::SwitchArg
    verboseArg("v", "verbose", "be verbose", false);
  cmdLine.add(verboseArg);
  TCLAP::UnlabeledMultiArg<string>
    dirsArg("directory...", "index specified dirs", false, "directory");
  cmdLine.add(dirsArg);
  cmdLine.parse(subArgs);

  if (chdirArg.getValue() != ".") {
    if (chdir(chdirArg.getValue().c_str()) == -1)
      return errno;
  }
  
  vector<string> exts = extArg.getValue();
  if (exts.empty())
    exts.push_back(".org");
  auto verbose = verboseArg.getValue();
  
  try {
    Xapian::TermGenerator indexer;
    Xapian::Stem stemmer(langArg.getValue());
    indexer.set_stemmer(stemmer);
    
    auto dirs = dirsArg.getValue();
    for (auto dir : dirs) {
      struct stat sb;
      // Whether a readable and writable directory.
      if ((stat(dir.c_str(), &sb) == 0) && S_ISDIR(sb.st_mode) &&
	  (access(dir.c_str(), R_OK|W_OK) != -1)) {
	if (verbose)
	  cerr << "indexing directory " << dir << endl;
	
	string dbFile(file_join(dir, ".xapian-db"));
	Xapian::WritableDatabase db(dbFile,
				    resetArg.getValue() ?
				    Xapian::DB_CREATE_OR_OVERWRITE :
				    Xapian::DB_CREATE_OR_OPEN);

	map<string, int64_t> fsFiles, dbFiles;
	map<string, Xapian::docid> dbIds;
	
	vector<string> orgFiles;
	ls_org(orgFiles, dir, ".", exts);
	for (const string& file : orgFiles) { // `dir` relative `file`
	  auto filePath = file_join(dir, file);
	  struct stat sb;
	  if (stat(filePath.c_str(), &sb) == 0) {
	    fsFiles[filePath] = sb.st_mtime;
	  }
	}
	
	db.begin_transaction(false);
	
	for (Xapian::PostingIterator it = db.postlist_begin("");
	     it != db.postlist_end(""); ++it) {
	  auto docId = *it;
	  auto doc = db.get_document(docId);
	  auto filePath = doc.get_data();
	  auto t = time_deserialize(doc.get_value(DOC_MTIME));
	  // Overwrites any existing value of the same key, and thus
	  // there will be no dupes in `dbFiles`, even if the database
	  // should have some.
	  dbFiles[filePath] = t;
	  dbIds[filePath] = docId;
	}

	{
	  auto makeDoc = [&] (const pair<string, int64_t>& x) {
	    const string& filePath = x.first;
	    ifstream infile(filePath);
	    Xapian::Document doc;
	    doc.set_data(filePath);
	    doc.add_value(DOC_MTIME, time_serialize(x.second));
	    indexer.set_document(doc);
	    {
	      const string& fileBase = file_basename(filePath);
	      indexer.index_text(fileBase, 1, "F");
	    }
	    {
	      string fileExt = file_extension(filePath);
	      if (!fileExt.empty()) {
		fileExt = fileExt.substr(1);
	      }
	      fileExt = downcase(fileExt);
	      //doc.add_boolean_term("E" + fileExt);
	      //doc.add_boolean_term("ZE" + fileExt);
	      indexer.index_text_without_positions(fileExt, 0, "E");
	      //cerr << "ext: '" << fileExt << "'" << endl;
	    }
	    {
	      string line;
	      bool titleDone = false;
	      while (getline(infile, line)) {
		if (whitespace_p(line)) {
		  // skip blank line
		} if (!string_starts_with(line, "#")) {
		  // non Org header mode
		  if (!titleDone) {
		    indexer.index_text(line, 1, "S");
		    indexer.index_text(line, titleArg.getValue());
		    indexer.increase_termpos();
		  } else {
		    indexer.index_text(line);
		  }
		  while (getline(infile, line)) {
		    //cerr << "body line: '" << line << "'" << endl;
		    indexer.index_text(line);
		  }
		  break;
		} else if (string_starts_with(line, "#+TITLE:")) {
		  string s = line.substr(8);
		  indexer.index_text(s, 1, "S");
		  indexer.index_text(s, titleArg.getValue());
		  indexer.increase_termpos();
		  titleDone = true;
		} else if (string_starts_with(line, "#+KEYWORDS:") ||
			   string_starts_with(line, "#+FILETAGS:")) {
		  string s = line.substr(11);
		  indexer.index_text_without_positions(s, 0, "K");
		  indexer.index_text(s);
		  indexer.increase_termpos();
		} else {
		  // skip comment (or unknown property) line
		}
	      }
	    }
	    return doc;
	  };

	  auto addFile = [&] (const pair<string, int64_t>& x) {
	    if (verbose)
	      cerr << "indexing file " << x.first << endl;
	    Xapian::Document doc = makeDoc(x);
	    db.add_document(doc);
	  };

	  auto updateFile = [&] (const pair<string, int64_t>& x,
				 Xapian::docid docId) {
	    if (verbose)
	      cerr << "re-indexing file " << x.first << endl;
	    Xapian::Document doc = makeDoc(x);
	    db.replace_document(docId, doc);
	  };
	  
	  auto rmFile = [&] (const pair<string, int64_t>& x) {
	    if (verbose)
	      cerr << "de-indexing file " << x.first << endl;
	    auto docId = dbIds[x.first];
	    db.delete_document(docId);
	  };
	  
	  auto fi = fsFiles.cbegin();
	  auto di = dbFiles.cbegin();
	  for (;;) {
	    if (fi == fsFiles.cend()) {
	      // The remaining files have been deleted.
	      for ( ; di != dbFiles.cend(); ++di) {
		rmFile(*di);
	      }
	      break;
	    } else if (di == dbFiles.cend()) {
	      // The remaining files are new.
	      for ( ; fi != fsFiles.cend(); ++fi) {
		addFile(*fi);
	      }
	      break;
	    } else if ((*fi).first == (*di).first) {
	      if ((*fi).second != (*di).second) {
		// The file has been modified.
		updateFile(*fi, dbIds[(*di).first]);
	      }
	      fi++;
	      di++;
	    } else if ((*fi).first < (*di).first) {
	      // The file has been added.
	      addFile(*fi);
	      fi++;
	    } else if ((*fi).first > (*di).first) {
	      // The file has been deleted.
	      rmFile(*di);
	      di++;
	    } else {
	      throw Xapian::AssertionError("unexpected condition");
	    }
	  } // end `for`
	}
	
	db.commit_transaction();
      }
    }
  } catch (const Xapian::Error &e) {
    cerr << e.get_description() << endl;
    return 1;
  }

  return 0;
}

static int doSearch(vector<string> subArgs) {
  TCLAP::CmdLine cmdLine("Specify a query expression as a string.");
  TCLAP::ValueArg<string>
    langArg("l", "lang", "stemming language (e.g., 'en' or 'fi')",
	    false, "en", "language");
  cmdLine.add(langArg);
  TCLAP::ValueArg<string>
    queryArg("q", "query", "specifies a query string", false, "", "string");
  cmdLine.add(queryArg);
  TCLAP::ValueArg<int>
    countArg("c", "max-count", "maximum number of results", false, 0, "number");
  cmdLine.add(countArg);
  TCLAP::SwitchArg
    timeArg("t", "time-sort", "sort by modification time", false);
  cmdLine.add(timeArg);
  TCLAP::SwitchArg
    verboseArg("v", "verbose", "be verbose", false);
  cmdLine.add(verboseArg);
  TCLAP::SwitchArg
    flag_pure_not("n", "pure-not", "allow NOT", false);
  cmdLine.add(flag_pure_not);
  TCLAP::SwitchArg
    flag_boolean_any_case("a", "boolean-any-case",
			  "allow lowercase operators", false);
  cmdLine.add(flag_boolean_any_case);
  TCLAP::UnlabeledMultiArg<string>
    dirsArg("dir...", "specifies directories to search", false, "directory");
  cmdLine.add(dirsArg);
  cmdLine.parse(subArgs);
  
  auto maxDocCount = countArg.getValue();
  bool timeSort = timeArg.getValue();
  auto verbose = verboseArg.getValue();
  
  try {
    Xapian::Database db;
    auto dirs = dirsArg.getValue();
    int numDbFiles = 0;
    for (auto dir : dirs) {
      string dbFile(file_join(dir, ".xapian-db"));
      if (access(dbFile.c_str(), R_OK) != -1) {
	Xapian::Database dirDb(dbFile);
	db.add_database(dirDb);
	numDbFiles++;
	//cout << "Added database: " << db.get_description() << endl;
      }
    }
    if (numDbFiles == 0)
      return 0;
    
    Xapian::Enquire enquire(db);
    if (timeSort) // by modification time, descending
      enquire.set_sort_by_value(DOC_MTIME, true);

    Xapian::QueryParser qp;
    qp.add_prefix("ext", "E");
    qp.add_prefix("file", "F");
    qp.add_prefix("title", "S");
    qp.add_prefix("tag", "K");
    Xapian::Stem stemmer(langArg.getValue());
    Xapian::Query query;
    if (queryArg.getValue() == "") {
      query = Xapian::Query::MatchAll;
    } else {
      qp.set_stemmer(stemmer);
      qp.set_database(db);
      qp.set_stemming_strategy(Xapian::QueryParser::STEM_SOME);
      unsigned flags =
	Xapian::QueryParser::FLAG_DEFAULT |
	(flag_pure_not.getValue() ?
	 Xapian::QueryParser::FLAG_PURE_NOT : 0) |
	(flag_boolean_any_case.getValue() ?
	 Xapian::QueryParser::FLAG_BOOLEAN_ANY_CASE : 0);
      query = qp.parse_query(queryArg.getValue(), flags);
      if (verbose)
	cerr << "parsed query is: " << query.get_description() << endl;
    }
    enquire.set_query(query);

    int maxItems = (maxDocCount ? maxDocCount : db.get_doccount());
    Xapian::MSet matches = enquire.get_mset(0, maxItems);
    for (Xapian::MSetIterator i = matches.begin(); i != matches.end(); ++i) {
      cout << i.get_document().get_data() << endl;
    }
  } catch (const Xapian::Error &e) {
    cerr << e.get_description() << endl;
    return 1;
  }
  return 0;
}

static int doDump(vector<string> subArgs) {
  TCLAP::CmdLine cmdLine("Specify the directories for dumping.");
  TCLAP::UnlabeledMultiArg<string>
    dirsArg("dir...", "specifies directories to search", false, "directory");
  cmdLine.add(dirsArg);
  cmdLine.parse(subArgs);
  try {
    auto dirs = dirsArg.getValue();
    for (auto dir : dirs) {
      string dbFile(file_join(dir, ".xapian-db"));
      if (access(dbFile.c_str(), R_OK) != -1) {
	cout << "database " << dbFile << endl;
	Xapian::Database db(dbFile);
	
	for (auto it = db.metadata_keys_begin();
	     it != db.metadata_keys_end(); it++) {
	  auto mkey = *it;
	  auto mvalue = db.get_metadata(mkey);
	  cout << "metadata " << mkey << " = " << mvalue << endl;
	}

	for (auto it = db.allterms_begin();
	     it != db.allterms_end(); it++) {
	  cout << "term " << *it << endl;
	}
      }
    }
  } catch (const Xapian::Error &e) {
    cerr << e.get_description() << endl;
    return 1;
  }
  return 0;
}

int main(int argc, const char* argv[])
{
  if (argc <= 1) {
    usage();
    return 1;
  }

  string cmd(argv[1]);
  vector<string> args({ string(argv[0]) + " " + cmd });
  for (int i = 2; i < argc; i++)
    args.emplace_back(argv[i]);
  // for (auto s : args) cout << s << endl;
  
  if (cmd == "index") {
    return doIndex(args);
  } else if (cmd == "search") {
    return doSearch(args);
  } else if (cmd == "dump") {
    return doDump(args);
  } else if (cmd == "-h" || cmd == "--help") {
    usage();
    return 0;
  } else {
    usage();
    return 1;
  }
}

/*

  Copyright (C) 2017 Tero Hasu

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License as
  published by the Free Software Foundation; either version 2 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA

  See the file GPL-2 for the full text of the GNU GPL.

  */
