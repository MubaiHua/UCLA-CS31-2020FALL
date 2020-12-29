// Import duty calculation

#include <iostream>
#include <string>
using namespace std;

int main()
{
      // Get and validate cheese type

    cout << "Cheese type: ";
    string type;
    getline(cin, type);
    if (type == "")
    {
        cout << "---\nYou must enter a cheese type" << endl;
        return 1;
    }

      // Get and validate value

    cout << "Value: ";
    double value;
    cin >> value;
    cin.ignore(10000, '\n');
    if (value < 0)
    {
        cout << "---\nThe value must be positive" << endl;
        return 1;
    }

      // Get and validate importer

    cout << "Importer: ";
    string importer;
    getline(cin, importer);
    if (importer == "")
    {
        cout << "---\nYou must enter an importer" << endl;
        return 1;
    }

      // Product value cutoff points and duty rates

    const double BRACKET_CUTOFF_1   =  1000;
    const double BRACKET_CUTOFF_2   =  13000;
    const double RATE_1         = 0.011;
    const double RATE_2_BASIC   = 0.02;
    const double RATE_2_SPECIAL = 0.014;
    const double RATE_3         = 0.029;

      // Compute import duty

    double duty;

    if (value <= BRACKET_CUTOFF_1)
        duty = value * RATE_1;
    else
    {
          // Compute duty for the portion of value in first bracket

        duty = BRACKET_CUTOFF_1 * RATE_1;

          // Determine rate for second bracket

        double rate_2 = RATE_2_BASIC;
        if (type == "cheshire"  ||  type == "stilton")
            rate_2 = RATE_2_SPECIAL;

        if (value <= BRACKET_CUTOFF_2)
        {
              // Add duty for remainder of value (in 2nd bracket)

            duty += (value - BRACKET_CUTOFF_1) * rate_2;
        }
        else
        {
              // Add duty for the portion of value in 2nd bracket
              // and the remainder of value (in 3rd bracket)

            duty += (BRACKET_CUTOFF_2 - BRACKET_CUTOFF_1) * rate_2 +
                    (value - BRACKET_CUTOFF_2) * RATE_3;
        }   
    }

      // Print duty

    cout.setf(ios::fixed);
    cout.precision(2);
    cout << "---\nThe import duty for " << importer << " is $" << duty << endl;
}
