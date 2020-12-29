#include <iostream>
#include<string>
using namespace std;
int main() {
	cout << "Cheese type: ";
	string type = "";
	getline(cin, type);
	if (type == "") 	//if an empty string was provided for the cheese type, the program will print out the message and terminate
	{
		cout << "---" << endl;
		cout << "You must enter a cheese type" << endl;
		return 1;
	}

	cout << "Value: ";
	double value = 0;
	cin >> value;
	if (value <= 0) //if the value is not positive, the program will print out the message and terminate
	{
		cout << "---" << endl;
		cout << "The value must be positive" << endl;
		return 1;
	}

	cout << "Importer: ";
	string importer = "";
	cin.ignore(10000, '\n');
	getline(cin, importer);
	if (importer == "")	//if an empty string was provided for the importer, the program will print out the message and terminate
	{
		cout << "---" << endl;
		cout << "You must enter an importer" << endl;
		return 1;
	}

	double duty = 0;

	if (value <= 1000)
	{
		duty = value * 1.1 / 100;//for the first 1000 dollars in value, the duty will be 1.1 %
	}


	else if (value > 1000 && value <= 13000) //the calculation for the next $12000 value
	{
		if (type == "cheshire" || type == "stilton")
		{
			duty = 11 + (value - 1000) * 1.4 / 100;//the duty for cheshire or stilton beyond $1000 plus the $11 duty for the first $1000
		}
		else
		{
			duty = 11 + (value - 1000) * 2 / 100; //the duty for the rest of cheese beyond $1000 plus the $11 duty for the first $1000
		}
	}

	else
	{
		if (type == "cheshire" || type == "stilton")
		{
			duty = 11.0 + 168.0 + (value - 13000) * 2.9 / 100; //the combined duty first $13000 plus the duty beyond $13000 for cheshire/stilton
		}
		else
		{
			duty = 11.0 + 240.0 + (value - 13000) * 2.9 / 100;//the combined duty first $13000 plus the duty beyond $13000
		}
	}

	cout.setf(ios::fixed);
	cout.precision(2);
	cout << "---" << endl;
	cout << "The import duty for " << importer << " is $" << duty << endl;
	return 0;
}