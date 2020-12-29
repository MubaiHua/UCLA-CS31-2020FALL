#define _CRT_SECURE_NO_WARNINGS
#include <iostream>
#include <cassert>
#include <cstring>
#include <cctype>
using namespace std;
const int MAX_WORD_LENGTH = 20;
const int MAX_DOCUMENT_LENGTH = 250;

int cleanupRules(char wordin[][MAX_WORD_LENGTH + 1], char wordout[][MAX_WORD_LENGTH + 1], int nRules);
int determineScore(const char document[], const char wordin[][MAX_WORD_LENGTH + 1], const char wordout[][MAX_WORD_LENGTH + 1], int nRules);
void eliminateNonLetter(char wordin[][MAX_WORD_LENGTH + 1], char wordout[][MAX_WORD_LENGTH + 1], int nRules); //eliminate all characters that is not a letter
void toLowerCase(char wordin[][MAX_WORD_LENGTH + 1], char wordout[][MAX_WORD_LENGTH + 1], int nRules); //convert all characters to lower case
void eliminateEmptyWordin(char wordin[][MAX_WORD_LENGTH + 1], char wordout[][MAX_WORD_LENGTH + 1], int nRules); //eliminate all empty wordin
void eliminateSameWordinWordout(char wordin[][MAX_WORD_LENGTH + 1], char wordout[][MAX_WORD_LENGTH + 1], int nRules); //eliminate the word-rule that wordin and wordout are the same
void eliminateSameOneWordRule(char wordin[][MAX_WORD_LENGTH + 1], char wordout[][MAX_WORD_LENGTH + 1], int nRules); //for the one-word case, eliminate all the one-word rule and two-word rule that has the same wordin
void eliminateSameTwoWordRule(char wordin[][MAX_WORD_LENGTH + 1], char wordout[][MAX_WORD_LENGTH + 1], int nRules); //for the two-word case, eliminate all the two-word rule with the same wordin that has the same wordout
void eliminateEmptyMatchRule(char wordin[][MAX_WORD_LENGTH + 1], char wordout[][MAX_WORD_LENGTH + 1], int nRules); //Eliminate all the empty match rules in the documnent
int getRuturnValue(char wordin[][MAX_WORD_LENGTH + 1], char wordout[][MAX_WORD_LENGTH + 1], int nRules); //Calculate the return value for the function cleanupRules
void convertToAllLowerAndAllLetter(char documentString[]); //convert the string to all lower case and eliminate the nonletter characters
void convertDocumentToArray(char documentString[], char documentArray[][MAX_WORD_LENGTH + 1]); //convert a string to a array of strings, with each element seperated by a space character
int countScores(const char documentArray[][MAX_WORD_LENGTH + 1], const char wordin[][MAX_WORD_LENGTH + 1], const char wordout[][MAX_WORD_LENGTH + 1], int nRules);

int main()
{
	/*int nRules=12;
	char wordin[13][MAX_WORD_LENGTH + 1] = { "confusion","FAMILY","charm","hearty","house","worn - out","family","charm","ties","","charm","Family","Hi" };
	char wordout[13][MAX_WORD_LENGTH + 1] = { "","TIES","confusion","hearty","intrigue","younger","first","","family","frightened","","tIeS","big"};

	cout << "original:" << endl;
	for (int i = 0; i < nRules; i++)
	{
		cout << i << ": " << wordin[i] << ", ";
	}
	cout << endl;
	for (int i = 0; i < nRules; i++)
	{
		cout << i << ": " << wordout[i] << ", ";
	}
	cout << endl;
	cout << "result:" << endl;
	cout << "return value: " << cleanupRules(wordin, wordout, nRules) << endl;
	for (int i = 0; i < nRules; i++)
	{
		cout << i << ": " << wordin[i] << ", ";
	}
	cout << endl;
	for (int i = 0; i < nRules; i++)
	{
		cout << i << ": " << wordout[i] << ", ";
	}
	cout << endl;
	return 0;*/
	const int TEST1_NRULES = 3;
	char test1win[TEST1_NRULES][MAX_WORD_LENGTH + 1] = {
		"family", "unhappy", "horse",
	};
	char test1wout[TEST1_NRULES][MAX_WORD_LENGTH + 1] = {
		"",       "horse",   "",
	};
	assert(determineScore("Happy families are all alike; every unhappy family is unhappy in its own way.",
		test1win, test1wout, TEST1_NRULES) == 2);
	assert(determineScore("Happy horses are all alike; every unhappy horse is unhappy in its own way.",
		test1win, test1wout, TEST1_NRULES - 1) == 0);
	assert(determineScore("Happy horses are all alike; every unhappy horse is unhappy in its own way.",
		test1win, test1wout, TEST1_NRULES) == 1);
	assert(determineScore("A horse!  A horse!  My kingdom for a horse!",
		test1win, test1wout, TEST1_NRULES) == 1);
	assert(determineScore("horse:stable ratio is 10:1",
		test1win, test1wout, TEST1_NRULES) == 0);
	assert(determineScore("**** 2020 ****",
		test1win, test1wout, TEST1_NRULES) == 0);
	cout << "All tests succeeded" << endl;
	return 0;
}

int cleanupRules(char wordin[][MAX_WORD_LENGTH + 1], char wordout[][MAX_WORD_LENGTH + 1], int nRules)
{
	if (nRules <= 0) //if nRules is zero or negative, return 0;
	{
		return 0;
	}
	eliminateNonLetter(wordin, wordout, nRules); //eliminate all characters that is not a letter
	toLowerCase(wordin, wordout, nRules); //convert all characters to lower case
	eliminateEmptyWordin(wordin, wordout, nRules); //eliminate all empty wordin
	eliminateSameWordinWordout(wordin, wordout, nRules); //eliminate the word-rule that wordin and wordout are the same
	eliminateSameOneWordRule(wordin, wordout, nRules); //for the one-word case, eliminate all the one-word rule and two-word rule that has the same wordin
	eliminateSameTwoWordRule(wordin, wordout, nRules); //for the two-word case, eliminate all the two-word rule with the same wordin that has the same wordout
	eliminateEmptyMatchRule(wordin, wordout, nRules); //Eliminate all the empty match rules in the documnent
	return getRuturnValue(wordin, wordout, nRules); //return the valve calculated by the function getRuturnValue
}

int determineScore(const char document[], const char wordin[][MAX_WORD_LENGTH + 1], const char wordout[][MAX_WORD_LENGTH + 1], int nRules)
{
	if (nRules <= 0) //if nRules is zero or negative, return 0;
	{
		return 0;
	}
	char documentString[MAX_DOCUMENT_LENGTH + 1]; //create a local sting
	strcpy(documentString, document); //store document into that local string
	convertToAllLowerAndAllLetter(documentString); //convert the string to all lower case and eliminate the nonletter characters
	char documentArray[MAX_DOCUMENT_LENGTH + 1][MAX_WORD_LENGTH + 1]; 
	convertDocumentToArray(documentString, documentArray); //convert a string to a array of strings, with each element seperated by a space character
	return countScores(documentArray, wordin, wordout, nRules); //return the score calculated by the fuction countScores
}

void eliminateNonLetter(char wordin[][MAX_WORD_LENGTH + 1], char wordout[][MAX_WORD_LENGTH + 1], int nRules)
{
	for (int i = 0; i < nRules; i++) //Go through the whole array
	{
		for (int j = 0; wordin[i][j] != '\0'; j++)
		{
			if (!isalpha(wordin[i][j])) //if the current string contains a nonletter, replace the current string to empty string 
			{
				strcpy(wordin[i], "");
			}
		}
	}
	for (int i = 0; i < nRules; i++) //Go through the whole array
	{
		for (int j = 0; wordout[i][j] != '\0'; j++)
		{
			if (!isalpha(wordout[i][j])) //if the currecnt string contains a nonletter, replace the current string to empty string 
			{
				strcpy(wordout[i], "");
			}
		}
	}
}

void toLowerCase(char wordin[][MAX_WORD_LENGTH + 1], char wordout[][MAX_WORD_LENGTH + 1], int nRules)
{
	for (int i = 0; i < nRules; i++) //Go through the whole array
	{
		for (int j = 0; wordin[i][j] != '\0'; j++) //convert every characer in the string to lower case
		{
			wordin[i][j] = tolower(wordin[i][j]);
		}
	}
	for (int i = 0; i < nRules; i++) //Go through the whole array
	{
		for (int j = 0; wordout[i][j] != '\0'; j++) //convert every characer in the string to lower case
		{
			wordout[i][j] = tolower(wordout[i][j]);
		}
	}
}

void eliminateEmptyWordin(char wordin[][MAX_WORD_LENGTH + 1], char wordout[][MAX_WORD_LENGTH + 1], int nRules)
{
	for (int i = 0; i < nRules; i++) //Go through every string in the array
	{
		if (strcmp(wordin[i], "") == 0) //if the current string is empty
		{
			strcpy(wordout[i], ""); //assign the corresponding wordout to be empty
		}
	}
}

void eliminateSameWordinWordout(char wordin[][MAX_WORD_LENGTH + 1], char wordout[][MAX_WORD_LENGTH + 1], int nRules)
{
	for (int i = 0; i < nRules; i++) //Go through every string in the array
	{
		if (strcmp(wordin[i], wordout[i]) == 0) //if the current word rule has the same wordin and wordout
		{
			strcpy(wordin[i], ""); //set wordin to empty
			strcpy(wordout[i], ""); //set wordout to empty
		}
	}
}

void eliminateSameOneWordRule(char wordin[][MAX_WORD_LENGTH + 1], char wordout[][MAX_WORD_LENGTH + 1], int nRules)
{
	char current[MAX_WORD_LENGTH + 1];
	for (int i = 0; i < nRules; i++) //go through all the strings in wordin
	{
		if (strcmp(wordout[i], "") == 0 && strcmp(wordin[i], "") != 0) //if the current word rule is a one-word rule
		{
			strcpy(current, wordin[i]); //assign current to the current string in wordin
			for (int j = 0; j < nRules; j++) //search through the strings in the array
			{
				if (strcmp(current, wordin[j]) == 0) //if there is a string in wordin the same as the current one, set both this string in wordin and its corresponding string in wordout to empty
				{
					strcpy(wordin[j], "");
					strcpy(wordout[j], "");
					strcpy(wordin[i], current); //reassign the current to the current sting
				}
			}
		}
	}
}

void eliminateSameTwoWordRule(char wordin[][MAX_WORD_LENGTH + 1], char wordout[][MAX_WORD_LENGTH + 1], int nRules)
{
	for (int i = 0; i < nRules; i++) //go through all the strings in wordin 
	{
		if (strcmp(wordout[i], "") != 0 && strcmp(wordin[i], "") != 0) //if the current word rule is a two-word rule
		{
			for (int j = i + 1; j < nRules; j++) //search through the strings in the array
			{
				if (strcmp(wordin[i], wordin[j]) == 0 && strcmp(wordout[i], wordout[j]) == 0) //if there is a two-word rule has the same wordin and wordout as the current one
				{
					strcpy(wordin[j], ""); //assign this two-word rule's wordin and wordout to be empty
					strcpy(wordout[j], "");
				}
			}
		}
	}
}

void eliminateEmptyMatchRule(char wordin[][MAX_WORD_LENGTH + 1], char wordout[][MAX_WORD_LENGTH + 1], int nRules)
{
	char tempIn[MAX_WORD_LENGTH + 1];
	char tempOut[MAX_WORD_LENGTH + 1];
	for (int i = nRules - 1; i > 0; i--) //Use bubble sort to move all the empty string to the back
	{
		for (int j = 0; j < i; j++)
		{
			if (strcmp(wordout[j], "") == 0 && strcmp(wordin[j], "") == 0) 
			{
				strcpy(tempIn, wordin[j]);
				strcpy(tempOut, wordout[j]);
				strcpy(wordin[j], wordin[j+1]);
				strcpy(wordout[j], wordout[j+1]);
				strcpy(wordin[j + 1], tempIn);
				strcpy(wordout[j + 1], tempOut);
			}
		}
	}
}

int getRuturnValue(char wordin[][MAX_WORD_LENGTH + 1], char wordout[][MAX_WORD_LENGTH + 1], int nRules)
{
	for (int i = 0; i < nRules; i++) //go through all the strings in both wordin array and and wordout array
	{
		if (strcmp(wordout[i], "") == 0 && strcmp(wordin[i], "") == 0) //return the index in the array that both wordin and wordout is empty
		{
			return i;
		}
	}
	return nRules;
}

void convertToAllLowerAndAllLetter(char documentString[])
{
	char stringWithOnlyLetters[MAX_DOCUMENT_LENGTH + 1] = ""; //initialize a new empty string
	int i, j;
	for (i = 0, j = 0; documentString[i] != '\0'; i++) //Go through documentString
	{
		if (isalpha(documentString[i]) || isblank(documentString[i])) //if the current character is a letter
		{
			stringWithOnlyLetters[j] = tolower(documentString[i]); //convert that character to lower case and store it into stringWithOnlyLetters
			j++;
		}
	}
	strcpy(documentString, stringWithOnlyLetters);
}

void convertDocumentToArray(char documentString[], char documentArray[][MAX_WORD_LENGTH + 1])
{
	int j = 0;
	int k = 0;
	int l;
	for (int x = 0; x < MAX_DOCUMENT_LENGTH + 1; x++) //make sure documentArray is empty
	{
		for (int y = 0; y < MAX_WORD_LENGTH + 1; y++)
		{
			documentArray[x][y] = '\0';
		}
	}
	for (int i = 0; documentString[i] != '\0'; i++)
	{
		if (isblank(documentString[i])) //store every word between space into documentArray
		{
			for (l = 0; j < i; j++, l++)
			{
				documentArray[k][l] = documentString[j];
			}
			j = i + 1;
			k++;
		}
	}
	for (l = 0; documentString[j] != '\0'; j++, l++) //assign the last word into documentArray
	{
		documentArray[k][l] = documentString[j];
	}
}

int countScores(const char documentArray[][MAX_WORD_LENGTH + 1], const char wordin[][MAX_WORD_LENGTH + 1], const char wordout[][MAX_WORD_LENGTH + 1], int nRules)
{
	int score = 0;
	bool match = false;

	match = false;
	for (int i = 0; i < nRules; i++)
	{
		for (int j = 0; strcmp(documentArray[j], "") != 0; j++) //Go through the words in documentArray
		{
			if (strcmp(documentArray[j], wordin[i]) == 0) //if the current wordin is the same as the current word in documentArray
			{
				match = true;
				for (int k = 0; strcmp(documentArray[k], "") != 0; k++) //Go through the words in documentArray
				{
					if (strcmp(documentArray[k], wordout[i]) == 0) //If the current word can find the same wordout
					{
						match = false;
					}
				}
			}
		}
		if (match == true)
		{
			score++; //count the match
			match = false;
		}
	}

	return score;
}