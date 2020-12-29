//  Project 3:  Poll Position

#include <string>
#include <cctype>
using namespace std;

bool isValidUppercaseStateCode(string stateCode);

//*************************************
//  isSyntacticallyCorrect
//*************************************

bool isSyntacticallyCorrect(string pollData)
{
      // An empty poll data string is syntactically correct

    if (pollData.size() == 0)
	return true;

      // Each iteration of the loop recognizes one state forecast

    size_t k = 0;
    while (k != pollData.size())
    {
	  // The forecast must start with a digit

	if (! isdigit(pollData[k]))
	    return false;
	k++;

	  // There might be a second digit

	if (k != pollData.size()  &&  isdigit(pollData[k]))
	    k++;

	  // Next must come two letters

	if (! isalpha(pollData[k]))
	    return false;
	k++;
	if (k == pollData.size()  ||  ! isalpha(pollData[k]))
	    return false;
	k++;

	  // Those letters must be the code for a state

	string state = pollData.substr(k-2, 2);
	state[0] = toupper(state[0]);
	state[1] = toupper(state[1]);
	if (! isValidUppercaseStateCode(state))
	    return false;

	  // Next must come a party code

	if (k == pollData.size()  ||  ! isalpha(pollData[k]))
	    return false;
        k++;
    }

      // We get here only if we got through pollData without a problem.

    return true;

}

//*************************************
//  tallyVotes
//*************************************

int tallyVotes(string pollData, char party, int& voteTally)
{
      // Define return values

    const int RET_OK             = 0;
    const int RET_BAD_SYNTAX     = 1;
    const int RET_BAD_PARTY      = 2;
    const int RET_BAD_STATE_VOTE = 3;

      // A bad party character prevents tallying

    if (!isalpha(party))
	return RET_BAD_PARTY;

      // A pollData string with incorrect syntax prevents tallying

    if (!isSyntacticallyCorrect(pollData))
	return RET_BAD_SYNTAX;

      // We will later compare party codes in uppercase, so adjust party

    party = toupper(party);

      // We will tally votes in an int named result, and modify the voteTally
      // parameter only if processing the entire pollData string succeeds.

    int result = 0;

      // Each iteration of the loop deals with one state forecast.  Since we
      // know at this point the pollData string has correct syntax, we are
      // guaranteed there are one or two digits, etc.

    size_t k = 0;
    while (k != pollData.size())
    {
	  // Determine the state vote tally

	int stateVoteTally = pollData[k] - '0';  // we know this is a digit
	k++;
	if (isdigit(pollData[k]))  // Is there a second digit?
	{
		stateVoteTally = 10 * stateVoteTally + pollData[k] - '0';
		k++;
	}

	  // The state vote tally must not be 0

	if (stateVoteTally == 0)
	    return RET_BAD_STATE_VOTE;

	  // Skip over the state code (we know there must be one)

	k += 2;

	  // If the party code (we know there must be one) matches, record
	  // the votes 

	if (toupper(pollData[k]) == party)
		result += stateVoteTally;
	k++;
    }

      // We've successfully processed the entire string, so set voteTally.

    voteTally = result;

    return RET_OK;
}

//*************************************
//  isValidUppercaseStateCode
//*************************************

// Return true if the argument is a two-uppercase-letter state code, or
// false otherwise.

bool isValidUppercaseStateCode(string stateCode)
{
    const string codes =
	"AL.AK.AZ.AR.CA.CO.CT.DE.DC.FL.GA.HI.ID.IL.IN.IA.KS."
        "KY.LA.ME.MD.MA.MI.MN.MS.MO.MT.NE.NV.NH.NJ.NM.NY.NC."
        "ND.OH.OK.OR.PA.RI.SC.SD.TN.TX.UT.VT.VA.WA.WV.WI.WY";
    return (stateCode.size() == 2  &&
	    stateCode.find('.') == string::npos  &&  // no '.' in stateCode
	    codes.find(stateCode) != string::npos);  // match found
}
