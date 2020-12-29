#include<iostream>
#include<string>
#include <cctype>
#include <cassert>
using namespace std;

bool isSyntacticallyCorrect(string pollData);
int tallyVotes(string pollData, char party, int& voteTally);
bool isValidUppercaseStateCode(string stateCode);
bool hasOnlyLetterAndNumber(string pollData); //Check whether the poll data string contains only letters and numbers
string toUpper(string pollData); //Convert the poll data string to all upper cases
bool isValidStateForecast(string pollData); //Check whether each state forcast in the poll data string is valid;
bool notZeroForecastPredicts(string pollData); //Check whether there are zero forecast predicts
int countVote(string pollData, char party); //Count the total number of votes for the party
int stringToInt(string str); //Convert string to int

int main()
{
	assert(isSyntacticallyCorrect("38TXR55CAD"));
	assert(!isSyntacticallyCorrect("38MXR55CAD"));
	assert(!isSyntacticallyCorrect("MXR55CAD"));
	assert(!isSyntacticallyCorrect("38MXA55CAD"));
	assert(!isSyntacticallyCorrect("38MXR55CAD2"));
	assert(!isSyntacticallyCorrect("38MXCAD"));
	assert(!isSyntacticallyCorrect("1MX2R55CAdD"));
	assert(!isSyntacticallyCorrect("38MXr55caD"));
	assert(!isSyntacticallyCorrect("38 MXr55caD"));
	assert(isSyntacticallyCorrect("38TXR55CAD6Msr29nYd06UTL"));
	int votes;
	votes = -999;
	assert(tallyVotes("38TXR55CAD6Msr29nYd06UTL", 'd', votes) == 0 && votes == 84);
	votes = -999;
	assert(tallyVotes("38TXR55CAD", '%', votes) == 2 && votes == -999);
	votes = -999;
	assert(tallyVotes("38TXR55CAD", '%', votes) == 2 && votes == -999);
	votes = -999;
	assert(tallyVotes("", '%', votes) == 2 && votes == -999);
	votes = -999;
	assert(tallyVotes("", 'r', votes) == 0 && votes == 0);
	votes = -999;
	assert(tallyVotes("0TXR55CAD", 'r', votes) == 3 && votes == -999);
	cout << "All tests succeeded" << endl;
	return 0;
}
bool isSyntacticallyCorrect(string pollData)
{
	string data = pollData; //Store pollData to a local variable
	if (data.empty()) //First check whether the pollData string is empty
	{
		return true;
	}
	if (!hasOnlyLetterAndNumber(data)) //Check whether the poll data string contains only letters and numbers
	{
		return false;
	}
	data = toUpper(data);//Convert the string to all upper cases
	if (!isValidStateForecast(data)) //Check whether the pollData string confirms to the format of proper poll data string
	{
		return false;
	}
	return true; //if the pollData string passes all cases above, return true
}

int tallyVotes(string pollData, char party, int& voteTally)
{
	string data = pollData; //Store pollData to a local variable
	data = toUpper(data);
	if (!isSyntacticallyCorrect(data)) //call the function isSyntacticallyCorrect to check whether pollData string is synatactically correct, if it is not, return 1
	{
		return 1;
	}
	if (!isalpha(party)) //Check whether the party code is a letter, if it is not, return 2
	{
		return 2;
	}
	if (!notZeroForecastPredicts(data)) //Use whether there are zero forecast predicts, if it is, return 3;
	{
		return 3;
	}
	if (data.empty()) //if the pollData string is empty, set voteTally to be 0 and return 0
	{
		voteTally = 0;
		return 0;
	}
	voteTally = countVote(data, party); //Count the total number of votes for the party and store the number in voteTally
	return 0;
}

bool isValidUppercaseStateCode(string stateCode)
{
	const string codes =
		"AL.AK.AZ.AR.CA.CO.CT.DE.DC.FL.GA.HI.ID.IL.IN.IA.KS."
		"KY.LA.ME.MD.MA.MI.MN.MO.MS.MT.NE.NV.NH.NJ.NM.NY.NC."
		"ND.OH.OK.OR.PA.RI.SC.SD.TN.TX.UT.VT.VA.WA.WV.WI.WY";
	return (stateCode.size() == 2 &&
		stateCode.find('.') == string::npos &&  // no '.' in stateCode
		codes.find(stateCode) != string::npos);  // match found
}

bool hasOnlyLetterAndNumber(string pollData)
{
	for (int i = 0; i != pollData.size(); i++) //Check character by character whether the string contains only letters and number
	{
		if (!isalpha(pollData.at(i)) && !isdigit(pollData.at(i)))
		{
			return false;
		}
	}
	return true;
}

string toUpper(string pollData)
{
	string data = pollData;
	for (int i = 0; i != data.size(); i++) //Convert every character in the string to upper case
	{
		data.at(i) = toupper(data.at(i));
	}
	return data;
}

bool isValidStateForecast(string pollData)
{
	string data = pollData;
	int i = 0;
	while (i != data.size()) //Do the checking process for every state forecast
	{
		string stateCode = "";
		string electoralVotes = "";
		int lessThanTwo = 1; //a variable that count the letters we take in, it only takes two letters for state code;
		string partyCode = "";
		while (i != data.size() && isdigit(data.at(i))) //first takes the digit in the string till it reaches soemthing else
		{
			electoralVotes += data.at(i);
			i++;
		}
		if (electoralVotes.size() == 0 || electoralVotes.size() > 2) //if there is no digits or more than 2 digits, the pollData string is not valid
		{
			return false;
		}
		while (i != data.size() && isalpha(data.at(i)) && lessThanTwo <= 2) //takes the following 2 letter from the pollData string and store it to the string stateCode
		{
			stateCode += data.at(i);
			i++;
			lessThanTwo++;
		}
		if (!isValidUppercaseStateCode(stateCode)) //checks whether the stateCode is valid
		{
			return false;
		}
		while (i != data.size() && isalpha(data.at(i))) //takes the folloing letters until it reaches something else, store the letters in the string partyCode
		{
			partyCode += data.at(i);
			i++;
		}
		if (partyCode != "R" && partyCode != "D" && partyCode != "G" && partyCode != "L") //check whether the party code is valid
		{
			return false;
		}
	}
	return true;
}

bool notZeroForecastPredicts(string pollData)
{
	int i = 0;
	while (i != pollData.size()) {
		string predicts = "";
		while (i != pollData.size() && isalpha(pollData.at(i))) //skip the letters in the pollData
			i++;
		while (i != pollData.size() && isdigit(pollData.at(i))) //takes the consecutive numbers in the pollData string and store them to the string predicts
		{
			predicts += pollData.at(i);
			i++;
		}
		if (predicts == "0" || predicts == "00") //If the predicts string is "0" or "00", it means that the predictions are 0
		{
			return false;
		}
	}
	return true;
}

int countVote(string pollData, char party)
{
	int i = 0;
	int totalVotes = 0;
	party = toupper(party);
	while (i != pollData.size())
	{
		string votes = "";
		while (i != pollData.size() && isdigit(pollData.at(i))) //takes the consecutive numbers in the pollData string and store them in the string votes
		{
			votes += pollData.at(i);
			i++;
		}
		i += 2; //skip the following 2 letters as state code
		if (i != pollData.size() && (pollData.at(i) == party)) // if the following party code is the party that we count
		{
			totalVotes += stringToInt(votes);	//convert the string votes to type int and add it into totalVotes
		}
		i++; //pass the party code
	}
	return totalVotes;
}

int stringToInt(string str)
{
	int value = 0;
	if (str.size() == 2) //the string of size 2 and size 1 have different way of conversion
	{
		value = (str.at(0) - '0') * 10 + (str.at(1) - '0');
	}
	else
	{
		value = str.at(0) - '0';
	}
	return value;
}