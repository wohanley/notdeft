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
    if (!string_starts_with(name, ".") &&
	!string_starts_with(name, "#")) {
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
  
void ls_org(vector<string>& res, const string& root) {
  for (const string& file : ls(root)) {
    auto absFile = file_join(root, file);
    if (string_ends_with(file, ".org")) {
      res.push_back(file);
    } else if (file_directory_p(absFile)) {
      vector<string> subLst = ls(absFile);
      for (const string& subFile : subLst) {
	if (string_ends_with(subFile, ".index.org")) {
	  res.push_back(file_join(file, subFile));
	  break;
	}
      }
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

static int doIndex(vector<string> subArgs) {
  TCLAP::CmdLine cmdLine("Specify the directories to index.");
  TCLAP::ValueArg<string>
    langArg("l", "lang", "stemming language (e.g., 'en' or 'fi')",
	    false, "en", "language");
  cmdLine.add(langArg);
  TCLAP::UnlabeledMultiArg<string>
    dirsArg("directory...", "index specified dirs", false, "directory");
  cmdLine.add(dirsArg);
  cmdLine.parse(subArgs);

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
	ls_org(orgFiles, dir);
	for (const string& file : orgFiles) {
	  //cout << "indexing file " << file << endl;
	  
	  ifstream infile(file_join(dir, file));
	  Xapian::Document doc;
	  doc.set_data(file);
	  indexer.set_document(doc);
	  for (string line; getline(infile, line); ) {
	    //cout << "line: '" << line << "'" << endl;
	    indexer.index_text(line);
	  }
	  db.add_document(doc);
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
  TCLAP::UnlabeledMultiArg<string>
    dirsArg("dir...", "specifies directories to search", false, "directory");
  cmdLine.add(dirsArg);
  cmdLine.parse(subArgs);
  try {
    Xapian::Database db;
    auto dirs = dirsArg.getValue();
    for (auto dir : dirs) {
      string dbFile(file_join(dir, ".xapian-db"));
      if (access(dbFile.c_str(), R_OK) != -1) {
	Xapian::Database dirDb(dbFile);
	db.add_database(dirDb);
      }
    }
    Xapian::Enquire enquire(db);
    Xapian::QueryParser qp;
    Xapian::Stem stemmer(langArg.getValue());
    qp.set_stemmer(stemmer);
    qp.set_database(db);
    qp.set_stemming_strategy(Xapian::QueryParser::STEM_SOME);
    Xapian::Query query = qp.parse_query(queryArg.getValue());
    //cout << "Parsed query is: " << query.get_description() << endl;
    enquire.set_query(query);

    Xapian::MSet matches = enquire.get_mset(0, db.get_doccount());
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
