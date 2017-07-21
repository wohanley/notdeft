#include <string.h>
#include <iostream>
#include <tclap/CmdLine.h>
#include <xapian.h>

using namespace std;

static void usage()
{
  cerr << "deft-xapian" << endl;
  cerr << "USAGE:" << endl;
  cerr << "To build/refresh search indices" << endl;
  cerr << "(for specified directories):" << endl;
  cerr << "  deft-xapian index [options] directory..." << endl;
  cerr << "To find text documents" << endl;
  cerr << "(matching the specified query):" << endl;
  cerr << "  deft-xapian search [options] term..." << endl;
}

static int doIndex(vector<string> subArgs) {
  TCLAP::CmdLine cmdLine("Specify the directories to index.");
  TCLAP::UnlabeledMultiArg<string>
    multi("directory...", "index specified dirs", false, "directory");
  cmdLine.add(multi);
  cmdLine.parse(subArgs);
  return 0;
}

static int doSearch(vector<string> subArgs) {
  TCLAP::CmdLine cmdLine("Specify required search terms.");
  TCLAP::UnlabeledMultiArg<string>
    multi("term...", "search for specified terms", true, "term");
  cmdLine.add(multi);
  cmdLine.parse(subArgs);
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
