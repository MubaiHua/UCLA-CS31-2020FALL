// Project 5 solution

  // The following line makes Visual C++ shut up; clang++ and g++ ignore it.
#define _CRT_SECURE_NO_WARNINGS

#include <cstring>
#include <cctype>
using namespace std;

const int MAX_WORD_LENGTH = 20;
const int MAX_DOCUMENT_LENGTH = 250;

  // Convert word to lower case, returning true if the word is non-empty
  // and contains only letters.
bool cleanupWord(char s[])
{
    for (int k = 0; s[k] != '\0'; k++)
    {
        if (isalpha(s[k]))
            s[k] = tolower(s[k]);
        else // Any non-alphabetic character means the word is bad.
            return false;
    }
    return true;  // Everything was OK
}

void rulecopy(char toin[], char toout[], const char fromin[], const char fromout[])
{
    if (toin != fromin)
        strcpy(toin, fromin);
    if (toout != fromout)
        strcpy(toout, fromout);
}

bool isWordInList(const char words[][MAX_WORD_LENGTH+1+1], int nWords, const char w[])
{
    for (int pos = 0; pos < nWords; pos++)
    {
        if (strcmp(w, words[pos]) == 0)
            return true;
    }
    return false;
}

int cleanupRules(char wordin[][MAX_WORD_LENGTH+1],
                 char wordout[][MAX_WORD_LENGTH+1],
                 int nRules)
{
    if (nRules <= 0)
        return 0;

      // Eliminate rules that are bad without regard to other rules

    int r = 0;
    while (r < nRules)
    {
          // Is win nonempty, and there are no bad chars in the words, and win
          // and wout aren't the same word?
        if (wordin[r][0] != '\0'  &&  cleanupWord(wordin[r])  &&
                cleanupWord(wordout[r])  &&  strcmp(wordin[r], wordout[r]) != 0)
            r++;  // Go on to the next rule
        else
        {
              // Copy the last rule into this one.  Don't increment r,
              // so that we examine that rule on the next iteration.
            rulecopy(wordin[r], wordout[r], wordin[nRules-1], wordout[nRules-1]);
            nRules--;
        }
    }

      // Partition rules so that all one-word rules come before all two-word
      // rules;
    int firstTwoWord = nRules-1;
    r = 0;
    while (r <= firstTwoWord)
    {
          // Advance r to next non-one-word rule
        while (r <= firstTwoWord  &&  wordout[r][0] == '\0')
            r++;

          // Back up firstTwoWord to next non-two-word rule
        while (r <= firstTwoWord  &&  wordout[firstTwoWord][0] != '\0')
            firstTwoWord--;

          // If found both out of position
        if (r < firstTwoWord)
        {
              // Swap them
            char tempin[MAX_WORD_LENGTH+1];
            char tempout[MAX_WORD_LENGTH+1];
            rulecopy(tempin, tempout, wordin[r], wordout[r]);
            rulecopy(wordin[r], wordout[r], wordin[firstTwoWord], wordout[firstTwoWord]);
            rulecopy(wordin[firstTwoWord], wordout[firstTwoWord], tempin, tempout);
              // Advance r, back up firstTwoWord
            r++;
            firstTwoWord--;
        }
    }
    firstTwoWord = r;

      // Now all rules from 0 to firstTwoWord-1 are one-word rules, and
      // all rules from firstTwoWord to nRules-1 are two-word rules.
      // The following loops always preserve this property.

      // One-word rules eliminate all others with the same win
    for (int r = 0; r < firstTwoWord; r++)
    {
          // Eliminate two-word rules with the same win
        for (int r2 = nRules-1; r2 >= firstTwoWord; r2--)
        {
            if (strcmp(wordin[r2], wordin[r]) == 0)
            {
                rulecopy(wordin[r2], wordout[r2], wordin[nRules-1], wordout[nRules-1]);
                nRules--;
            }
        }

          // Eliminate one-word rules with the same win
        for (int r2 = firstTwoWord-1; r2 > r; r2--)
        {
            if (strcmp(wordin[r2], wordin[r]) == 0)
            {
                  // Preserve the property that all one-word rules come before
                  // all two-word rules by moving the last one-word rule onto
                  // the rule we're eliminating, and moving the last two-word
                  // rule to where the last one-word rule was
                rulecopy(wordin[r2], wordout[r2], wordin[firstTwoWord-1], wordout[firstTwoWord-1]);
                firstTwoWord--;
                rulecopy(wordin[firstTwoWord], wordout[firstTwoWord], wordin[nRules-1], wordout[nRules-1]);
                nRules--;
            }
        }
    }

      // Two-word rules eliminate all other two-word rules with the same win
      // and wout
    for (int r = firstTwoWord; r < nRules; r++)
    {
        for (int r2 = nRules-1; r2 > r; r2--)
        {
            if (strcmp(wordin[r2], wordin[r]) == 0  &&  strcmp(wordout[r2], wordout[r]) == 0)
            {
                rulecopy(wordin[r2], wordout[r2], wordin[nRules-1], wordout[nRules-1]);
                nRules--;
            }
        }
    }

    return nRules;
}

int determineScore(const char document[],
                   const char wordin[][MAX_WORD_LENGTH+1],
                   const char wordout[][MAX_WORD_LENGTH+1],
                   int nRules)
{
      // Make a list of document words

      // There can't be more than this many words.  (Worst case is a
      // document of one-letter words like "a a a ... a"
    const int MAX_DOC_WORDS = (1+MAX_DOCUMENT_LENGTH) / 2;

      // We'll store the document words here.  If a document word is more
      // than MAX_WORD_LENGTH letters long, we'll store only the first
      // MAX_WORD_LENGTH+1 letters; since the long word can't possibly
      // match a rule word (which is limited to MAX_WORD_LENGTH
      // characters), we'll store only enough to ensure we don't match.
    char docWord[MAX_DOC_WORDS][MAX_WORD_LENGTH+1+1];

      // Visit each character of the document, transferring letters into
      // the docWord array.
    int nDocWords = 0;
    int docWordPos = 0;
    for (int pos = 0; document[pos] != '\0'; pos++)
    {
        if (isalpha(document[pos]))
        {
              // Append letter to the end of the current docWord
            if (docWordPos < MAX_WORD_LENGTH+1)
            {
                docWord[nDocWords][docWordPos] = tolower(document[pos]);
                docWordPos++;
            }
        }
        else if (document[pos] == ' ')
        {
              // If a word was started, mark the end and prepare for the next 
            if (docWordPos > 0)
            {
                docWord[nDocWords][docWordPos] = '\0';
                nDocWords++;
                docWordPos = 0;
            }
        }
          // Non-letter, non-blank characters aren't transferred and don't
          // end a word, so do nothing with them.
    }
      // If the document didn't end with a blank, end the last word
    if (docWordPos > 0)
    {
        docWord[nDocWords][docWordPos] = '\0';
        nDocWords++;
    }

      // Count matching rules

    int nMatches = 0;

      // For each rule
    for (int r = 0; r < nRules; r++)
    {
          // If win is in the document
        if (isWordInList(docWord, nDocWords, wordin[r]))
        {
              // It's a match if the rule is a one-word rule, or if it's
              // a two-word rule and wout is not in the document
            if (wordout[r][0] == '\0'  ||  ! isWordInList(docWord, nDocWords, wordout[r]))
                nMatches++;
        }
    }
    return nMatches;
}
