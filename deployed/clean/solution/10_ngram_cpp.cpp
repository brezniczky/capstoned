// [[Rcpp::plugins("cpp11")]]

#include <Rcpp.h>
#include <string>
#include <sys/stat.h>
#include <iostream>
#include <fstream>
using namespace Rcpp;
using namespace std;

const int
  INVALID_MAX = -1,
  INVALID_WORD = -1,
  INVALID_SENTENCE_ENDING = -2;

// TODO: should have gone the tokenize - cache - collect n-grams way
// TODO: this is becoming more a word processing than just an n-gram file

// this trie implementation is from
// http://www.sourcetricks.com/2011/06/c-tries.html#.VwxeASYtihc
// modified by Janos Brezniczky (brezniczky@gmail.com)
class Node {
public:
  Node() { mContent = ' '; mMarker = false; }
  ~Node() {
    vector<Node *>::iterator it = mChildren.begin();
    while (it != mChildren.end()) {
      delete(*it);
      ++it;
    }
  };
  char content() { return mContent; }
  void setContent(char c) { mContent = c; }
  bool wordMarker() { return mMarker; }
  void setWordMarker() { mMarker = true; }
  Node* findChild(char c);
  void appendChild(Node* child) { mChildren.push_back(child); }
  vector<Node*> children() { return mChildren; }

private:
  char mContent;
  bool mMarker;
  vector<Node*> mChildren;
};

class Trie {
public:
  Trie();
  ~Trie();
  void addWord(const string &s);
  bool searchWord(const string &s);
private:
  Node* root;
};

Node* Node::findChild(char c)
{
  for ( int i = 0; i < mChildren.size(); i++ )
  {
    Node* tmp = mChildren.at(i);
    if ( tmp->content() == c )
    {
      return tmp;
    }
  }

  return NULL;
}

Trie::Trie()
{
  root = new Node();
}

Trie::~Trie()
{
  delete(root);
}

void Trie::addWord(const string &s)
{
  Node* current = root;

  if ( s.length() == 0 )
  {
    current->setWordMarker(); // an empty word
    return;
  }

  for ( int i = 0; i < s.length(); i++ )
  {
    Node* child = current->findChild(s[i]);
    if ( child != NULL )
    {
      current = child;
    }
    else
    {
      Node* tmp = new Node();
      tmp->setContent(s[i]);
      current->appendChild(tmp);
      current = tmp;
    }
    if ( i == s.length() - 1 )
      current->setWordMarker();
  }
}

bool Trie::searchWord(const string &s)
{
  Node* current = root;

  while ( current != NULL )
  {
    for ( int i = 0; i < s.length(); i++ )
    {
      Node* tmp = current->findChild(s[i]);
      if ( tmp == NULL )
        return false;
      current = tmp;
    }

    if ( current->wordMarker() )
      return true;
    else
      return false;
  }

  return false;
}

void note_ngram(int ngram_n, int word_ids[], map<vector<int>, int> &freqs) {
  for(int i = ngram_n - 1; i >= 0; i--) {
    if (word_ids[i] <= INVALID_WORD) {
      return;
    }
  }

  vector<int> key;

  for(int i = 0; i < ngram_n; i++) {
    key.push_back(word_ids[i]);
  }

  map<vector<int>, int>::iterator it = freqs.find(key);
  if (it != freqs.end()) {
    it -> second += 1;
  }
  else {
    freqs.insert(pair<vector<int>, int>(key, 1));
  }
}

// tried using accelerator tables (256-element, although that seemed to match
// the char type) but somehow created bugs so the safe character classification
// is as below:
bool is_good_char(const char c) {
  return(
    ((c >= 'a') && (c <= 'z')) ||
    ((c >= 'A') && (c <= 'Z'))
  );
}

bool is_sentence_ending(const char c) {
  // sentence endings are special separator characters which instead of being
  // ignored as content, count as invalid words
  return(
    (c == '.') ||
    (c == '!') ||
    (c == '?') ||
    (c == '\n')
  );
}

bool is_standalone_char(const char c) {
  return((c == '&') ||
         is_sentence_ending(c));
}

bool is_grey_char(const char c) {
  return(
    ((c >= '0') && (c <= '9')) ||
    ((c == '\'') && (c == '`'))
  );
}

bool is_separator(const char c) {
  return(
    !
      (
          ((c >= 'a') && (c <= 'z')) ||
            ((c >= 'A') && (c <= 'Z')) ||
            ((c >= '0') && (c <= '9')) ||
            (c == '\'') ||
            (c == '`') ||
            (c == '&')
      )
  );
}

bool is_valid_word(const string &word) {

  if ((word.size() == 1) &&
      (is_standalone_char(word[0]))) {

    return(!is_sentence_ending(word[0]));
  }

  // the word must not contain anything but alpha characters, or those
  // and some numbers, maybe some apostrophies, & signs
  bool has_good_chars = false;
  bool has_grey_chars = false;

  for(int i = 0; i < word.length(); i ++) {
    if (is_good_char(word.at(i))) {
      has_good_chars = true;
    }
    else if (is_grey_char(word.at(i))) {
      return(false);
    }
  }

  return(has_good_chars);
}

// [[Rcpp::export]]
string standardize_sentence_first(const string &word) {
  // surnames and first names will be obviously misspelled, however, they are
  // likely to appear inside sentences, too, hopefully more often
  // TODO: confirm prevalence of such words

  string lower_word;
  bool locase_present = false;

  for(int i = 0; (i < word.length()); i ++) {
    if ((word[i] >= 'a') && (word[i] <= 'z')) {
      locase_present = true;
      break;
    }
  }

  if (locase_present) {
    lower_word.assign(word);
    lower_word[0] = tolower(lower_word[0]);
    return(lower_word);
  }
  else {
    return(word);
  }
}

// [[Rcpp::export]]
string standardize_word(const string &word, bool is_first_in_sentence) {
  // converts the word to the 'unified' representation - i.e.
  // - apostrophes become the standard "'"
  // - changes casing to lowercase using simple heuristic if
  //    is_first_in_sentence is specified
  // - fixes the "I'm" construct: always to be cased "I'm"

  if ((word.length() == 1) && (is_sentence_ending(word[0]))) {
    return(word);
  }

  string standard_word, lower_std_word;
  int len = word.length();

  if (word == "&") {
    return("and");
  }

  standard_word.assign(word);

  for(int i = 0; i < len; i++) {
    if (standard_word[i] == '`') { // or (standard_word[i] == "’"[0])) {
      standard_word[i] = '\'';
    }
  }

  int pos;
  string wrong_apo = "’";
  while(string::npos != (pos = standard_word.find(wrong_apo))) {
    standard_word.replace(pos, strlen(wrong_apo.c_str()), "\'");
  }

  if (strcasecmp("I'm", standard_word.c_str()) == 0) {
    return("I'm");
  }

  if (is_first_in_sentence) {
    return(standardize_sentence_first(standard_word));
  }

  return(standard_word);
}

// TODO: excluded_words is really misleading, rename: ignored_words
// TODO: switch to the consistent use of token or *term* instead of word
// TODO: switch to OOP - it's an obvious need and an obvious option
// TODO: document with a DFA graph
bool get_next_word(const string &line,
                   int &start_pos,
                   string &word) {
  int j;

//   printf("get_next_word is called with: %s\n",
//          line.substr(start_pos, 10).c_str());

  for(j = start_pos; j < line.size(); j++) {
    if (!is_separator(line[j])) break;
    if (is_sentence_ending(line[j])) {
      word = line[j];
      start_pos = j + 1; // point to the character succeeding the word
      return(true);
    }
  }

  // word start found
  start_pos = j;

  if ((j < line.size()) &&
      (is_standalone_char(line[j]))) {
    word = line[j];
    ++ start_pos;
    return(true);
  }

  for(j = start_pos; j < line.size(); j++) {
    if (is_separator(line[j])) {
      break;
    }
  }

  if (j > start_pos) {
    word = line.substr(start_pos, j - start_pos); //.c_str()
    start_pos = j;
    return(true);
  }

  return(false);
}

template<typename T> void create_csv_output(
    const map<string, int> &word_ids,
    const map<T, int> &freqs,
    const string &words_filename, const string &freqs_filename,
    int ngram_n) {
  // it seems using the "endl" manipulator is very slow on my test platform,
  // so I'll use "\n" instead (everything understands \n by now anyway in a
  // CSV) (a likely explanation found on the web is that endl flushes the
  // stream so actual and frequent, costly disk writes are triggered
  const char endlc = '\n';

  if (words_filename != "") {
    printf("preparing dictionary...\n");
    // create dictionary csv file
    vector<string> words(word_ids.size());
    for(map<string, int>::const_iterator it = word_ids.begin();
        it != word_ids.end(); it ++) {
      words[it -> second] = it -> first;
    }

    printf("writing dictionary...\n");
    ofstream words_fs(words_filename.c_str(), ofstream::out);

    words_fs << "word" << endlc;
    for(unsigned int i = 0; i < word_ids.size(); i ++) {
      words_fs << words[i] << endlc;
    }
    words_fs.close();
  }

  printf("writing freqs...\n");
  // create frequency csv file
  ofstream freqs_fs(freqs_filename.c_str(), ofstream::out);
  for(int col = 0; col < ngram_n; col ++) {
    freqs_fs << "word_" << col + 1 << ",";
  }
  freqs_fs << "freq" << endlc;

  for(typename map<T, int>::const_iterator it = freqs.begin();
      it != freqs.end();
      it++) {

    T vec = it->first; // TODO: this creates a copy - do something!
    for(int col = 0; col < ngram_n; col ++) {
      freqs_fs << vec[col] << ",";
    }
    freqs_fs << it->second << endlc;
  }
  freqs_fs.close();
  printf("freqs written\n");
}

template<typename T> List create_R_output(
    const map<string, int> &word_ids,
    const map<T, int> &freqs, int ngram_n) {
  // create output
  vector<string> r_words(word_ids.size());

  map<string, int>::const_iterator it = word_ids.begin();
  while (it != word_ids.end()) {
    r_words.at(it -> second) = it -> first;
    it++;
  }

  IntegerMatrix r_freqs(Dimension(freqs.size(), ngram_n + 1));
  typename map<T, int>::const_iterator it_2 = freqs.begin();
  int r_freq_i = 0;
  while (it_2 != freqs.end()) {
    T vec = (it_2 -> first); // TODO: this creates a copy - do something!
    for(int i = 0; i < ngram_n; i++) {
      r_freqs.at(r_freq_i, i) = vec[i];
    }
    r_freqs.at(r_freq_i, ngram_n) = it_2 -> second;

    it_2++;
    r_freq_i++;
  }

  return(List::create(Named("words") = r_words,
                      Named("freqs") = r_freqs));
}

// [[Rcpp::export]]
void test() {
  string word("oneword");
  bool good_chars[256], grey_chars[256], separators[256];

  if (!is_good_char('a')) {
    printf("good letter test failed!\n");
  }

  if (!is_valid_word(word)) {
    printf("good word test failed!\n");
  }
}

int get_word_id(string word, map<string, int> &word_ids) {
  int word_id;

  if (is_valid_word(word)) {
    map<string, int>::iterator word_it = word_ids.find(word);
    if (word_it != word_ids.end()) {
      word_id = word_it -> second;
    }
    else {
      word_id = word_ids.size();
      word_ids.insert(pair<string, int>(word, word_id));
    }
  }
  else {
    if ((word.size() == 1) && is_sentence_ending(word[0])) {
      word_id = INVALID_SENTENCE_ENDING;
    }
    else {
      word_id = INVALID_WORD;
    }
  }

  return(word_id);
}

// [[Rcpp::export]]
vector<string> get_words(const vector<string> &text, bool standardize) {
  if (text.size() > 1) {
    stop("Only a single line is supported.");
  }

  int text_pos = 0;

  list<string> word_list;
  string word;

  while(get_next_word(text[0], text_pos, word)) {
    if (standardize) {
      word = standardize_word(word, false);
    }
    word_list.push_back(word);
  }

  vector<string> result(word_list.size());

  list<string>::iterator it = word_list.begin();
  int i = 0;
  while(it != word_list.end()) {
    result[i] = *it;
    it ++;
    i++;
  }

  return(result);
}

typedef void (*word_callback_type)(const int word_id, void* user);

string tolower(const string &word) {
  // char* result = new char[word.length()];
  string result(word);
  for(int i = static_cast<int>(word.length()); i >= 0; i--) {
    result[i] = tolower(word[i]);
  };
  return(result);
}

// [[Rcpp::export]]
vector<string> tolowerR(const vector<string> &word) {
  vector<string> R_result;
  R_result.push_back(tolower(word[0]));
  return(R_result);
}

void get_word_ids_template(const vector<string> &text,
                           const vector<string> &excluded_words,
                           const vector<string> &enabled_words,
                           bool use_enabled_words,
                           map<string, int> &word_ids,
                           word_callback_type word_callback,
                           void* user) {
  // excluded_words: if a word is listed then it is ignored, as if they were
  //                 removed from the text prior to the analysis
  //
  //                 verified case insensitively
  //
  // enabled_words: if a (non-excluded) word is not listed here then counts as
  //                invalid (i.e. will break n-gram validity, too)
  Trie excluded_words_set;
  for(int i = 0; i < excluded_words.size(); i++) {
    excluded_words_set.addWord(excluded_words[i]);
  }
  Trie enabled_words_set;
  for(int i = 0; i < enabled_words.size(); i++) {
    enabled_words_set.addWord(enabled_words[i]);
  }

  map<vector<int>, int> freqs;

  string line = text[0];

  for(int i = 0; i < line.size(); ) {

    string word;
    if (!get_next_word(line, i, word)) {
      break;
    }

//     printf("word retrieved:%s\n", word.c_str());

    word = standardize_word(word, false);

//     printf("standardized word:%s\n", word.c_str());

    // force the word into the dictionary even if it's not enabled etc.
    // to get the same word_ID's when going through the text again with a new
    // set of excluded/enabled words
    //
    // (this is for 1..n-gram consistency - sorry, I should have tokenized once
    // and for all instead)
    int word_id = get_word_id(word, word_ids);

//     printf("word_id:%d\n", word_id);

//    printf("checking if word is excluded:%s\n", word.c_str());

    if (!excluded_words_set.searchWord(tolower(word))) {
      if ((word_id > INVALID_MAX) && use_enabled_words &&
          (!enabled_words_set.searchWord(word)))  {
        word_id = INVALID_WORD;
      }
//      printf("calling word_callback with:%d\n", word_id);
      word_callback(word_id, user);
    }
    else {
//       printf("was an excluded word: %s\n", word.c_str());
    }
  }
  // make sure a 'line end' is received on EOF (bit of a good willed hack)
  word_callback(INVALID_SENTENCE_ENDING, user);
}

struct int_pair {
  int x[2];

  int operator [] (int index) {
    return(x[index]);
  }

  bool operator == (const int_pair &rhs) {
    return((x[0] == rhs.x[0]) && (x[1] == rhs.x[1]));
  }

  bool operator < (const int_pair &rhs) const {
    return(
      x[0] != rhs.x[0] ?
    x[0] < rhs.x[0] :
    x[1] < rhs.x[1]
    );
  }

  int_pair() {
    x[0] = 0;
    x[1] = 0;
  }

  int_pair(const int_pair &copied) {
    x[0] = copied.x[0];
    x[1] = copied.x[1];
  }

  int_pair(int x0, int x1) {
    x[0] = x0;
    x[1] = x1;
  }
};

typedef
  struct {
    vector<int> sentence_word_ids;
    map<int_pair, int> counts;
  } associations_state;

typedef associations_state* p_associations_state;

void association_callback(int word_id, void* user) {
//   cout << "called with word id:" << word_id << endl;

  static int longest_line = 0;

  if (word_id == INVALID_WORD)
    return;

  p_associations_state state = static_cast<p_associations_state>(user);

  if (word_id != INVALID_SENTENCE_ENDING) {
    state->sentence_word_ids.push_back(word_id);
  }
  else {
//     printf("line processed\n");
    for(int i = 0; i < state->sentence_word_ids.size(); ++i) {
      for(int j = 0; j < state->sentence_word_ids.size(); ++j) {
        int id1 = state->sentence_word_ids[i];
        int id2 = state->sentence_word_ids[j];
        if (id1 < id2) {
          int_pair key(id1, id2);
          map<int_pair, int>::iterator it = state->counts.find(key);

          if (it == state->counts.end()) {
//             printf("adding count: %d %d 1\n", key[0], key[1]);
            state->counts.insert(pair<int_pair, int>(key, 1));
          }
          else {
//             printf("increased count: %d %d\n", key[0], key[1]);
            it->second++;
          };
        };
      }
    }
    state->sentence_word_ids.clear();
  }

  if (longest_line < state->sentence_word_ids.size()) {
    longest_line = state->sentence_word_ids.size();
    cout << "longest line:" << longest_line << "words" << endl;
  }
}

// [[Rcpp::export]]
List get_associations_cpp(const vector<string> &text,
                          const vector<string> &excluded_words,
                          const vector<string> &enabled_words,
                          bool use_enabled_words,
                          const vector<string> &csv_filenames) {

  map<string, int> word_ids;
  associations_state state;

  get_word_ids_template(
    text, excluded_words, enabled_words, use_enabled_words,
    word_ids, &association_callback, &state
  );

  string words_filename = csv_filenames[0];
  string freqs_filename = csv_filenames[1];

  List result;

  if (csv_filenames.size() > 1) {
    printf("csv_file generation...\n");
    string words_filename = csv_filenames[0];
    string freqs_filename = csv_filenames[1];
    create_csv_output<int_pair>(
      word_ids, state.counts, words_filename, freqs_filename, 2);
    result = List();
    printf("csv_file generation done\n");
  }
  else {
    result = create_R_output<int_pair>(word_ids, state.counts, 2);
  }
  return(result);
}

typedef
  struct {
    list<int> word_hist;
    map<vector<int>, int> counts;
  } ngrams_state;

typedef ngrams_state* p_ngrams_state;

void ngrams_callback(int word_id, void* user) {
//   cout << "ngrams_callback called with word id:" << word_id << endl;

  static int longest_line = 0;
  p_ngrams_state state = static_cast<p_ngrams_state>(user);

  state->word_hist.pop_front();
  state->word_hist.push_back(word_id);

  vector<int> key;
  // exit on any invalid component (including sentence endings)
  for(list<int>::iterator it = state->word_hist.begin();
      it != state->word_hist.end(); ++ it) {
    if (*it <= INVALID_MAX) {
      return;
    }
    key.push_back(*it);
  }

  map<vector<int>, int>::iterator counts_it = state->counts.find(key);
  if (counts_it == state->counts.end()) {
    state->counts.insert(pair<vector<int>, int>(key, 1));
  }
  else {
    ++ counts_it->second;
  }
}

// [[Rcpp::export]]
List get_ngrams_fast_cpp(const vector<string> &text,
                         const vector<string> &excluded_words,
                         const vector<string> &enabled_words,
                         int ngram_n,
                         bool use_enabled_words,
                         const vector<string> &csv_filenames) {
  // excluded_words: if a word is listed then it is ignored, just as if they
  //                 were removed from the text prior to the analysis
  //
  // enabled_words: if a (non-excluded) word is not listed here then counts as
  //                invalid (i.e. will break n-gram validity, too)

  // TODO: could replace the string keys with the c_str pointers perhaps
  // or could use a trie
  map<string, int> word_ids;
  ngrams_state state;
  for(int i = 0; i < ngram_n; ++i) {
    state.word_hist.push_back(INVALID_WORD);
  }

  get_word_ids_template(text, excluded_words, enabled_words, use_enabled_words,
                        word_ids, ngrams_callback, static_cast<void*>(&state));

  List result;
  if (csv_filenames.size() > 1) {
    printf("csv_file generation!\n");
    string words_filename = csv_filenames[0];
    string freqs_filename = csv_filenames[1];
    create_csv_output(word_ids, state.counts, words_filename, freqs_filename,
                      ngram_n);
  }
  else {
    result = create_R_output(word_ids, state.counts, ngram_n);
  }

  return(result);
}

// [[Rcpp::export]]
void test_create_csv_output() {
  map<string, int> word_ids;
  word_ids.insert(pair<string, int>("apple", 0));
  word_ids.insert(pair<string, int>("delta", 2));
  word_ids.insert(pair<string, int>("cider", 1));
  map<vector<int>, int> freqs;
  create_csv_output(word_ids, freqs, "data/temp/words.csv",
                    "data/temp/freqs.csv", 3);
}

// [[Rcpp::export]]
void test_standardize_word() {
  string word = "&";
  word = standardize_word(word, false);
  if (word != "and") {
    stop("test_standardize_word failed (1)");
  }

  map<string, int> word_ids;
  word_ids.insert(pair<string, int>("not", 3));
  word_ids.insert(pair<string, int>("and", 15));
  word_ids.insert(pair<string, int>("yes", 20));

  int id = get_word_id(word, word_ids);
  if (id != 15) {
    stop("test_canonize_word failed (2)");
  }
}

/***R
get_ngrams_fast = function(text, excluded_words, ngram_n,
                           enabled_words = NULL, csv_filenames = "") {

# boilerplate code to allow default value for enabled_words
  if (!is.null(enabled_words)) {
    return(get_ngrams_fast_cpp(text, excluded_words, enabled_words,
                               ngram_n, TRUE, csv_filenames))
  }
  else {
    return(get_ngrams_fast_cpp(text, excluded_words, c(""),
                               ngram_n,
                               FALSE, csv_filenames))
  }
}

get_associations = function(text, excluded_words, ngram_n,
                            enabled_words = NULL, csv_filenames = "") {

  # boilerplate code to allow default value for enabled_words
  if (!is.null(enabled_words)) {
    return(get_associations_cpp(text, excluded_words, enabled_words,
                               ngram_n, TRUE, csv_filenames))
  }
  else {
    return(get_associations_cpp(text, excluded_words, c(""),
                               ngram_n,
                               FALSE, csv_filenames))
  }
}
*/

/*** R
if (exists("run.tests")) if (run.tests) {
  test()
  print(get_ngrams_fast("I'm not what i'm not not not not not!", c("nothing"), ngram_n = 2))
  print(get_ngrams_fast("hello bello hello rock and roll roll is   not    deathed!!!", c(""), ngram_n = 2))
  print(get_ngrams_fast("a a a b b b b", c(""), ngram_n = 2))
  print(get_ngrams_fast("a a a b b b b", c(""), ngram_n = 3))
  print(get_ngrams_fast("b b b b", ngram_n = 3, excluded_words = c("a")))
  print(get_ngrams_fast("a a a b b b b", c(""), ngram_n = 3, enabled_words = c("a")))
  print(get_ngrams_fast("b b b b", ngram_n = 3, excluded_words = c("a")))
  print(get_ngrams_fast("b b b b", ngram_n = 3, excluded_words = c("a")))
  print(get_ngrams_fast("I`m not what i’m not not not not not!", c("nothing"), ngram_n = 2))


  res1 = get_ngrams_fast(text = "This is a badword text", excluded_words = "",
                         ngram_n = 1)

  res2 = get_ngrams_fast(text = "This is a badword text",
                         excluded_words = "badword", ngram_n = 1)

  res3 = get_ngrams_fast(text = "This is a badword text", excluded_words = "",
                         enabled_words = c("This", "is", "a", "text"), ngram_n = 1)

  res4 = get_ngrams_fast(text = "This is a badword text", excluded_words = "",
                         enabled_words = c("This", "is", "a", "text"), ngram_n = 3)

  if (!(identical(res1$words, res2$words) &&
      identical(res1$words, res3$words) &&
      identical(res1$words, res4$words))) {
    stop("ngram test failed")
  }
}

test_ngram_and_freqs = function() {
  print("test_ngram_and_freqs")
  res =
    get_ngrams_fast(text = "me and them & them and them and && rock&roll",
                    excluded_words = "",
                    ngram_n = 1,
                    csv_filenames = "")

  res.frame = data.frame(word = res$words[res$freqs[, 1] + 1], freq = res$freqs[, 2])
  print(res.frame)

  if (res.frame$freq[res.frame$word == "and"] != 6) {
    stop("test_ngram_and_freqs() failed")
  }
}

test_sentence_ending_becomes_invalid_word = function() {
  print("test_sentence_ending_becomes_invalid_word")
  # this indirectly means that below
  # "sentence. a" is not becoming a bigram ("sentence", "a")
  res =
    get_ngrams_fast(text = "this is a sentence. a good perfect sentence. we liek it all trhee.",
                    excluded_words = "",
                    ngram_n = 2)
  res.frame = data.frame(word_1 = res$words[res$freqs[, 1] + 1],
                         word_2 = res$words[res$freqs[, 2] + 1],
                         freq = res$freqs[, 3])

  if (sum(res.frame$word_1 == "sentence") != 0) {
    stop("test_sentence_ending_becomes_invalid_word() failed")
  }

  print(res.frame)
}

test_associations = function() {
  print("test_associations")
  res =
    get_associations_cpp(
      text = "hello world. an apple is as savoury as a lemon sometimes. life is live.",
      excluded_words = c("is", "an", "as", "a"),
      enabled_words = "",
      use_enabled_words = FALSE,
      csv_filenames = "")

  print(res)

  res =
    get_associations_cpp(
      text = "hello world hello world",
      excluded_words = c("is", "an", "as", "a"),
      enabled_words = "",
      use_enabled_words = FALSE,
      csv_filenames = "")

  print(res)

  res =
    get_associations_cpp(
      text = "hello. world. hello world. hello the world.",
      excluded_words = c("is", "an", "as", "a"),
      enabled_words = "",
      use_enabled_words = FALSE,
      csv_filenames = "")

  print(res)

# TODO: redundant case!
  res =
    get_associations_cpp(
      text = "hello world. an apple is as savoury as a lemon sometimes. life is live.",
      excluded_words = c("is", "an", "as", "a"),
      enabled_words = "",
      use_enabled_words = FALSE,
      csv_filenames = "")

  print(res)

  res =
    get_associations_cpp(
      text = "a big hello goes out to the hello world applications.",
      excluded_words = c("is", "an", "as", "a", "the"),
      enabled_words = "",
      use_enabled_words = FALSE,
      csv_filenames = "")

  print(res)

  res =
    get_associations_cpp(
      text = "a\nbig\nhello\ngoes\nout",
      excluded_words = c("is", "an", "as", "a", "the"),
      enabled_words = "",
      use_enabled_words = FALSE,
      csv_filenames = "")

  print(res)

  res =
    get_associations_cpp(
      text = "is A B",
      excluded_words = c("a", "is"),
      enabled_words = "",
      use_enabled_words = FALSE,
      csv_filenames = "")

  print(res)
}

{
# get_ngrams_fast(text = "Adam Sandler speaking Adam Sandler", excluded_words = "", ngram_n = 2)
# test_create_csv_output()
# test_standardize_word()
# test_sentence_ending_becomes_invalid_word()
# test_associations()
# test_ngram_and_freqs()
}
*/
