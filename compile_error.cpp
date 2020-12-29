// Code for Project 1
// Report poll results

#include <iostream>
using namespace std;

int main()
{
	int numberSurveyed;
	int forJoe;
	string forDonald; //I changed data type of "forDonald" from int to string

	cout << "How many registered voters were surveyed? ";
	cin >> numberSurveyed;
	cout << "How many of them say they will vote for Joe? ";
	cin >> forJoe;
	cout << "How many of them say they will vote for Donald? ";
	cin >> forDonald;

	double pctJoe = 100.0 * forJoe / numberSurveyed;
	double pctDonald = 100.0 * forDonald / numberSurveyed //I removed the semicolon at the end

	cout.setf(ios::fixed);
	cout.precision(1);

	cout << endl;
	cout << pctJoe << "% say they will vote for Joe." << endl;
	cout << pctDonald << "% say they will vote for Donald." << endl;

	if (forJoe > forDonald)
		cout << "Joe is predicted to win the election." << endl;
	else
		cout << "Donald is predicted to win the election." << endl;
}