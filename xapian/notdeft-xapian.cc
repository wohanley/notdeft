#include <algorithm>
#include <ctype.h>
#include <dirent.h>
#include <fstream>
#include <iostream>
#include <regex>
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

/** Returns the length of any note header marker such as "#" or "%#"
 * or "@;#". If the string is not a header string, returns 0. */
size_t string_header_marker_len(const string& s) {
  const size_t len = s.length();
  if (len >= 1) {
    if (s[0] == '#')
      return 1;
    if (len >= 2) {
      if ((s[1] == '#') && (s[0] == '%'))
	return 2;
      if (len >= 3) {
	if ((s[2] == '#') && (s[0] == '@') && (s[1] == ';'))
	  return 3;
	if (len >= 5) {
	  if ((s[4] == '#') && (s[0] == '<') && (s[1] == '!') &&
	      (s[2] == '-') && (s[3] == '-'))
	    return 5;
	}
      }
    }
  }
  return 0;
}

bool line_skip_marker(const string& s, size_t& pos) {
  const size_t len = string_header_marker_len(s);
  if (len == 0)
    return false;
  pos = len;
  return true;
}

/** Whether the lowercased string 's' matches 'pfx' starting at
 * position 'pos'. If so, increment 'pos' to index the position after
 * 'pfx'. */
bool string_lc_skip_keyword(const string& s,
			    size_t& pos,
			    const string& pfx) {
  auto pfx_len = pfx.length();
  auto epos = pos + pfx_len;
  if (s.length() < epos)
    return false;
  for (size_t i = 0; i < pfx_len; ++i) {
    if (tolower(s[pos + i]) != pfx[i])
      return false;
  }
  pos += pfx_len;
  return true;
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

/** Return the pathname of the parent directory of `s`, or return ""
    if `s` has no directory components, or if `s` is "/". */
string file_directory_path(const string& s) {
  auto found = s.find_last_of('/');
  if ((found == string::npos) || (found == 0))
    return "";
  return string(s.substr(0, found));
}

/** Return the non-directory component of pathname `s`, or return `s`
    itself if `s` has no directory components. */
string file_non_directory(const string& s) {
  auto found = s.find_last_of('/');
  if (found == string::npos)
    return s;
  return string(s.substr(found + 1));
}

/** Return the non-directory component of `s`, with its last extension
    (if any) removed. A filename that is "all extension" has no
    extension. */
string file_basename(const string& s) {
  auto basename = file_non_directory(s);
  size_t found = basename.find_last_of('.');
  if ((found == 0) || (found == string::npos))
    return basename;
  return string(basename.substr(0, found));
}

/** Return the last filename extension of `s`, with its leading ".",
    or return "" if `s` has no extension. A filename that is "all
    extension" has no extension. */
string file_extension(const string& s) {
  auto basename = file_non_directory(s);
  size_t found = basename.find_last_of('.');
  if ((found == 0) || (found == string::npos))
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

static bool uni_keyword_separator_p(const unsigned ch) {
  return (ch == ':') || (ch == ';') || (ch == ',')
    || Xapian::Unicode::is_whitespace(ch);
}

/** Expects an UTF-8 encoded line as the argument `s`,
    but reverts to octets for the remaining input if
    non-UTF-8 encoding is detected. */
static void uni_index_keywords(Xapian::TermGenerator& indexer,
			       const string& s) {
  Xapian::Utf8Iterator q(s);
  for (;;) {
    while (q.left() && uni_keyword_separator_p(*q)) q++;
    if (!q.left()) break;
    const char* const p = q.raw();
    while (q.left() && !uni_keyword_separator_p(*q)) q++;
    const string kw(p, q.raw());
    indexer.index_text(kw, 0, "K");
    indexer.increase_termpos();
    if (!q.left()) break;
  }
}

/** Takes a string containing org-mode links and returns a string containing
    just the titles of those links, e.g.
    "[[file:20200101_xapian.org][Xapian]] [[file:20200101_notdeft.org][NotDeft]]" -> {"Xapian" "NotDeft"} */
static vector<string> extract_link_titles(const string& links) {
  regex link_regex("\\[\\[([^\\[\\]]+)\\]\\[([^\\[\\]]+)\\]\\]", regex_constants::ECMAScript);
  string search = links;
	smatch smtch;
  vector<string> titles = {};
	while (regex_search(search, smtch, link_regex)) {
    titles.push_back(smtch[2]);
    search = smtch.suffix().str();
  }

  return titles;
}

struct Op {
  bool whole_dir;
  string dir;
  vector<string> files;
  Op() {}
  explicit Op(const string& d) : whole_dir(true), dir(d) {}
};

static bool parse_ops(istream& in, vector<Op>& lst) {
  string opcode;
  while (getline(in, opcode)) {
    if (opcode == ":idir") {
      string dir;
      if (getline(in, dir)) {
	lst.push_back(Op(dir));
      } else {
	return false; // expected directory name
      }
    } else if (opcode == ":ifiles") {
      string dir;
      if (!getline(in, dir))
	return false; // expected directory name
      string count_s;
      if (!getline(in, count_s))
	return false; // expected file count
      int count = std::stoi(count_s);
      if (count < 0)
	return false; // expected non-negative integer
      Op op;
      op.whole_dir = false;
      op.dir = dir;
      string file;
      for ( ; count > 0; count--) {
	if (!getline(in, file))
	  return false; // expected count filenames
	op.files.push_back(file);
      }
      lst.push_back(op);
    } else {
      return false; // unknown command
    }
  }
  return true;
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
static constexpr Xapian::valueno DOC_FILENAME = 1;

static int doIndex(vector<string> subArgs) {
  TCLAP::CmdLine cmdLine
    ("Specify any indexing commands via STDIN."
     " For each command, specify its database index directory."
     " All paths are used and stored as given."
     " Search results are reported with the stored paths,"
     " regardless of the search-time working directory.");
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
  TCLAP::SwitchArg
    inputArg("i", "input", "read instructions from STDIN", false);
  cmdLine.add(inputArg);
  TCLAP::UnlabeledMultiArg<string>
    dirsArg("directory...", "index specified dirs", false, "directory");
  cmdLine.add(dirsArg);
  cmdLine.parse(subArgs);

  if (chdirArg.getValue() != ".") {
    if (chdir(chdirArg.getValue().c_str()) == -1) {
      auto e = errno;
      cerr << "could not change into directory " <<
	chdirArg.getValue() << " (errno: " << e << ")" << endl;
      return 1;
    }
  }
  
  vector<string> exts = extArg.getValue();
  if (exts.empty())
    exts.push_back(".org");

  auto verbose = verboseArg.getValue();
  
  vector<Op> opList;
  {
    auto dirs = dirsArg.getValue();
    for (auto dir : dirs) {
      opList.push_back(Op(dir));
    }
  }
  if (inputArg.getValue()) {
    if (!parse_ops(cin, opList)) {
      cerr << "option -i / --input given, "
	"but failed to parse instructions from STDIN" << endl;
      if (verbose) { // print out parsed instructions
	cerr << "successfully parsed:" << endl;
	ostream& out(cerr);
	for (auto op : opList) {
	  out << op.dir;
	  if (op.whole_dir) {
	    out << endl << " (ALL)" << endl;
	  } else {
	    for (auto file : op.files) {
	      out << endl << " " << file;
	    }
	    out << endl;
	  }
	}
      }
      return 1;
    }
  }
  
  try {
    Xapian::TermGenerator indexer;
    Xapian::Stem stemmer(langArg.getValue());
    indexer.set_stemmer(stemmer);
    
    for (auto op : opList) {
      auto dir = op.dir;
      
      struct stat sb;
      // Whether a readable and writable directory.
      if ((stat(dir.c_str(), &sb) == 0) && S_ISDIR(sb.st_mode) &&
	  (access(dir.c_str(), R_OK|W_OK) != -1)) {
	if (verbose) {
	  cerr << "indexing directory " << dir << endl;
	}
	
	string dbFile(file_join(dir, ".notdeft-db"));
	Xapian::WritableDatabase db(dbFile,
				    resetArg.getValue() ?
				    Xapian::DB_CREATE_OR_OVERWRITE :
				    Xapian::DB_CREATE_OR_OPEN);

	map<string, int64_t> fsFiles; // mtimes for files in file system
	map<string, int64_t> dbFiles; // mtimes for files in database
	map<string, Xapian::docid> dbIds;
	
	vector<string> orgFiles;
	if (op.whole_dir) {
	  ls_org(orgFiles, dir, ".", exts);
	} else {
	  // Sparse directory paths must be specified relative to
	  // their database root.
	  orgFiles = op.files;
	}
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
	    const string fileNonDir = file_non_directory(filePath);
	    doc.add_value(DOC_FILENAME, fileNonDir);
	    indexer.set_document(doc);
	    {
	      const string fileDir = file_directory_path(filePath);
	      indexer.index_text(fileDir, 1, "P");
	    }
	    {
	      const string fileBase = file_basename(fileNonDir);
	      indexer.index_text(fileBase, 1, "F");
	    }
	    {
	      /* As for Omega, lowercase, without dot, and just "E"
		 for the no extension case. */ 
	      string fileExt = file_extension(fileNonDir);
	      if (!fileExt.empty()) {
		fileExt = downcase(fileExt.substr(1));
	      }
	      //doc.add_boolean_term("E" + fileExt);
	      indexer.index_text_without_positions(fileExt, 0, "E");
	      //cerr << "ext: '" << fileExt << "'" << endl;
	    }
	    {
        string tags_keyword = "- tags ::";
	      string line;
	      bool titleDone = false;
	      size_t pos = 0;
	      while (getline(infile, line)) {
		if (whitespace_p(line)) {
		  // skip blank line
		} else if (line.find(tags_keyword) != string::npos) {
      // index tags. tags list is not technically an org header, so this happens
      // outside of the check for that
      const string tags_text = line.substr(tags_keyword.length());
      vector<string> tags_titles = extract_link_titles(tags_text);
      for (string title : tags_titles) {
        indexer.index_text(title, 0, "K");
        indexer.increase_termpos();
      }
      indexer.index_text(tags_text);
      indexer.increase_termpos();
		} else if (!line_skip_marker(line, pos)) {
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
		} else if (string_lc_skip_keyword(line, pos, "+title:")) {
		  const string s = line.substr(pos);
		  indexer.index_text(s, 1, "S");
		  indexer.index_text(s, titleArg.getValue());
		  indexer.increase_termpos();
		  titleDone = true;
		} else if (string_lc_skip_keyword(line, pos, "+keywords:") ||
			   string_lc_skip_keyword(line, pos, "+filetags:")) {
		  const string s = line.substr(pos);
		  uni_index_keywords(indexer, s);
		  indexer.index_text(s);
		  indexer.increase_termpos();
		} else {
		  // skip comment (or unknown property) line
		}
	      }
	    }
	    return doc;
	  }; // end makeDoc

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
    nameArg("f", "name-sort", "sort by file name (overrides '-t')", false);
  cmdLine.add(nameArg);
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
  bool nameSort = nameArg.getValue();
  bool timeSort = timeArg.getValue();
  auto verbose = verboseArg.getValue();
  
  try {
    Xapian::Database db;
    auto dirs = dirsArg.getValue();
    int numDbFiles = 0;
    for (auto dir : dirs) {
      string dbFile(file_join(dir, ".notdeft-db"));
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
    if (nameSort) // by filename, descending
      enquire.set_sort_by_value(DOC_FILENAME, true);
    else if (timeSort) // by modification time, descending
      enquire.set_sort_by_value(DOC_MTIME, true);

    Xapian::QueryParser qp;
    qp.add_prefix("path", "P");
    qp.add_prefix("file", "F");
    qp.add_prefix("ext", "E");
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
