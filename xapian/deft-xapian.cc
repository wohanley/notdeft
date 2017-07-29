#include <dirent.h>
#include <fstream>
#include <iostream>
#include <string.h>
#include <sys/stat.h>
#include <tclap/CmdLine.h>
#include <unistd.h>
#include <xapian.h>

using namespace std;

bool string_starts_with(const string& s, const string& pfx) {
  return s.compare(0, pfx.length(), pfx) == 0;
}

bool string_ends_with(const string& s, const string& sfx) {
  const int pos = s.length() - sfx.length();
  return (pos >= 0) && (s.compare(pos, sfx.length(), sfx) == 0);
}

bool file_directory_p(const string& file) {
  struct stat sb;
  return (stat(file.c_str(), &sb) == 0) && S_ISDIR(sb.st_mode);
}

/** Returns an empty list on failure. */
vector<string> ls(const string& file) {
  vector<string> lst;
  DIR* dir = opendir(file.c_str()); // might use `unique_ptr` to close
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
  
void ls_org(vector<string>& res, const string& root,
	    const string& dir, const string ext) {
  auto absDir = file_join(root, dir);
  for (const string& file : ls(absDir)) {
    auto relFile = file_join(dir, file);
    auto absFile = file_join(absDir, file);
    if (string_ends_with(file, ext)) {
      res.push_back(relFile);
    } else if (file_directory_p(absFile)) {
      ls_org(res, root, relFile, ext);
    }
  }
}

static void usage()
{
  cerr << "deft-xapian" << endl;
  cerr << "USAGE:" << endl;
  cerr << "To build/refresh search indices" << endl;
  cerr << "(for specified directories):" << endl;
  cerr << "  deft-xapian index [options] directory..." << endl;
  cerr << "To find text documents" << endl;
  cerr << "(matching the specified query):" << endl;
  cerr << "  deft-xapian search [options] directory..." << endl;
}

static constexpr Xapian::valueno DOC_MTIME = 0;

static int doIndex(vector<string> subArgs) {
  TCLAP::CmdLine cmdLine
    ("Specify the directories to index."
     " Any relative paths are stored as given."
     " Search results are reported in the same manner, regardless of the search time working directory.");
  TCLAP::ValueArg<string>
    langArg("l", "lang", "stemming language (e.g., 'en' or 'fi')",
	    false, "en", "language");
  cmdLine.add(langArg);
  TCLAP::ValueArg<string>
    extArg("x", "extension", "filename extension (default: '.org')",
	    false, ".org", "extension");
  cmdLine.add(extArg);
  TCLAP::ValueArg<string>
    chdirArg("c", "chdir", "change working directory first",
	    false, ".", "directory");
  cmdLine.add(chdirArg);
  TCLAP::UnlabeledMultiArg<string>
    dirsArg("directory...", "index specified dirs", false, "directory");
  cmdLine.add(dirsArg);
  cmdLine.parse(subArgs);

  if (chdirArg.getValue() != ".") {
    if (chdir(chdirArg.getValue().c_str()) == -1)
      return errno;
  }
  
  auto ext = extArg.getValue();
  
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
	//cout << "indexing directory " << dir << endl;
	
	string dbFile(file_join(dir, ".xapian-db"));
	Xapian::WritableDatabase db(dbFile, Xapian::DB_CREATE_OR_OVERWRITE);
	db.begin_transaction(false);

	vector<string> orgFiles;
	ls_org(orgFiles, dir, ".", ext);
	for (const string& file : orgFiles) { // `dir` relative `file`
	  //cout << "indexing file " << file << endl;

	  auto filePath = file_join(dir, file);

	  struct stat sb;
	  if (stat(filePath.c_str(), &sb) == 0) {
	    ifstream infile(filePath);
	    Xapian::Document doc;
	    doc.set_data(filePath);
	    doc.add_value(DOC_MTIME,
			  Xapian::sortable_serialise(sb.st_mtime));
	    indexer.set_document(doc);
	    for (string line; getline(infile, line); ) {
	      //cout << "line: '" << line << "'" << endl;
	      indexer.index_text(line);
	    }
	    db.add_document(doc);
	  }
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
    queryArg("q", "query", "specifies a query string", true, "", "string");
  cmdLine.add(queryArg);
  TCLAP::ValueArg<int>
    countArg("c", "max-count", "maximum number of results", false, 0, "number");
  cmdLine.add(countArg);
  TCLAP::SwitchArg
    timeArg("t", "time-sort", "sort by modification time", false);
  cmdLine.add(timeArg);
  TCLAP::UnlabeledMultiArg<string>
    dirsArg("dir...", "specifies directories to search", false, "directory");
  cmdLine.add(dirsArg);
  cmdLine.parse(subArgs);
  auto maxDocCount = countArg.getValue();
  bool timeSort = timeArg.getValue();
  try {
    Xapian::Database db;
    auto dirs = dirsArg.getValue();
    for (auto dir : dirs) {
      string dbFile(file_join(dir, ".xapian-db"));
      if (access(dbFile.c_str(), R_OK) != -1) {
	Xapian::Database dirDb(dbFile);
	db.add_database(dirDb);
	//cout << "Added database: " << db.get_description() << endl;
      }
    }
    Xapian::Enquire enquire(db);
    if (timeSort) // by modification time, descending
      enquire.set_sort_by_value(DOC_MTIME, true);
    Xapian::QueryParser qp;
    Xapian::Stem stemmer(langArg.getValue());
    qp.set_stemmer(stemmer);
    qp.set_database(db);
    qp.set_stemming_strategy(Xapian::QueryParser::STEM_SOME);
    Xapian::Query query = qp.parse_query(queryArg.getValue());
    //cout << "Parsed query is: " << query.get_description() << endl;
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
