#include <iostream>
#include <string>
#include <cassert>
using namespace std;

int appendToAll(string a[], int n, string value);
int lookup(const string a[], int n, string target);
int positionOfMax(const string a[], int n);
int rotateLeft(string a[], int n, int pos);
int countRuns(const string a[], int n);
int flip(string a[], int n);
int differ(const string a1[], int n1, const string a2[], int n2);
int subsequence(const string a1[], int n1, const string a2[], int n2);
int lookupAny(const string a1[], int n1, const string a2[], int n2);
int divide(string a[], int n, string divider);

int main()
{
	string h[7] = { "martha", "mark", "joe", "susan", "", "kamala", "lindsey" };
	assert(lookup(h, 7, "kamala") == 5);
	assert(lookup(h, 7, "joe") == 2);
	assert(lookup(h, 2, "joe") == -1);
	assert(positionOfMax(h, 7) == 3);

	string g[4] = { "martha", "mark", "lindsey", "sara" };
	assert(differ(h, 4, g, 4) == 2);
	assert(appendToAll(g, 4, "?") == 4 && g[0] == "martha?" && g[3] == "sara?");
	assert(rotateLeft(g, 4, 1) == 1 && g[1] == "lindsey?" && g[3] == "mark?");

	string e[4] = { "joe", "susan", "", "kamala" };
	assert(subsequence(h, 7, e, 4) == 2);

	string d[5] = { "mark", "mark", "mark", "susan", "susan" };
	assert(countRuns(d, 5) == 2);

	string f[3] = { "lindsey", "joe", "mike" };
	assert(lookupAny(h, 7, f, 3) == 2);
	assert(flip(f, 3) == 3 && f[0] == "mike" && f[2] == "lindsey");

	assert(divide(h, 7, "lindsey") == 3);

	cout << "All tests succeeded" << endl;
	return 0;
}

int appendToAll(string a[], int n, string value)
{
	if (n < 0) //if n is negative, return -1
	{
		return -1;
	}
	for (int i = 0; i < n; i++) //append value to the end of each of the n elements of the array
	{
		a[i] = a[i] + value;
	}
	return n;
}

int lookup(const string a[], int n, string target)
{
	if (n < 0) //if n is negative, return -1
	{
		return -1;
	}
	for (int i = 0; i < n; i++) //Go through each element in the array
	{
		if (a[i] == target) //if the string in the array mathches target, return its position
		{
			return i;
		}
	}
	return -1; //if there is no such string, return -1
}

int positionOfMax(const string a[], int n)
{
	if (n <= 0) //if n is negative or zero, return -1
	{
		return -1;
	}
	string max = a[0]; //assign the first element of the array to max
	int index;
	for (int i = 0; i < n; i++) //Go through each element in the array
	{
		if (a[i] >= max) //if the current element in the array is larger than max, assign it to max
		{
			max = a[i];
		}
	}
	index = lookup(a, n, max);
	return index;
}

int rotateLeft(string a[], int n, int pos)
{
	if (n <= 0 || pos >= n || pos<0) //return -1 for notwithstanding behavior
	{
		return -1;
	}
	string elementAtPos = a[pos];
	for (int i = pos; i < n - 1; i++) //go through the elements from pos
	{
		a[i] = a[i + 1]; //assign the next element to current element
	}
	a[n - 1] = elementAtPos; //assign the last element t
	return pos;
}

int countRuns(const string a[], int n)
{
	int total = 0;
	if (n < 0) //if n is negative, return -1
	{
		return -1;
	}
	int i = 0;
	while (i < n) //go through the elements in the array
	{
		if ((i<n-1) && (a[i + 1] == a[i]) ) //pass the consecutive identital elements
		{
			i++;
		}
		else
		{
			i++;
			total++;
		}
	}
	return total;
}

int flip(string a[], int n)
{
	if (n < 0) //if n is negative, return -1
	{
		return -1;
	}
	string temp;
	for (int i = 0; i < n / 2; i++) //go through the elements in the array till i reaches half of the array's size
	{
		temp = a[i]; //store the current element in temp
		a[i] = a[n - 1 - i]; //assign the corresponding element to the current element
		a[n - 1 - i] = temp; //assign the temp to the corresponding element
	}
	return n;
}

int differ(const string a1[], int n1, const string a2[], int n2)
{
	if (n1 < 0 || n2 < 0) //if n1 or n2 is negative, return -1
	{
		return -1;
	}
	int i = 0;
	while (i < n1 && i < n2 && a1[i] == a2[i]) //go through the element untill corresponding elements of a1 and a2 that are not equal or i is larger than n1 or n2
	{
		i++; //count the position
	}
	return i;
}

int subsequence(const string a1[], int n1, const string a2[], int n2)
{
	if (n1 < 0 || n2 < 0) //if n1 or n2 is negative, return -1
	{
		return -1;
	}
	if (n2 == 0) //if a2 has size 0, return 0
	{
		return 0;
	}
	int pos;
	for (int i = 0; i < n1; i++) //go through array a1
	{
		bool consecutive = true;
		if (a1[i] == a2[0]) //find the index of the element in a1 that mathces the first element of a2
		{
			pos = i;

			for (int j = 0; j < n2; j++)
			{
				if ((i+j>=n1)||(a2[j] != a1[i + j])) //if the following element after i does not match n2, set pos to -1 and terminate the loop
				{
					pos = -1;
					consecutive = false;
					break;
				}
			}
			if(consecutive==true)
			{
				return pos; 
			}
		}
	}
	return -1;
}

int lookupAny(const string a1[], int n1, const string a2[], int n2)
{
	if (n1 <= 0 || n2 <= 0) //if n1 or n2 is negative, return -1
	{
		return -1;
	}
	for (int i = 0; i < n1; i++) // for each element in a1, search through a2 for euqal element
	{
		for (int j = 0; j < n2; j++)
		{
			if (a1[i] == a2[j])
			{
				return i;
			}
		}
	}
	return -1;
}

int divide(string a[], int n, string divider)
{
	if (n < 0) //if n is negative, return -1
	{
		return -1;
	}
	string temp;
	for (int i = 0; i < n - 1; i++) //sort the array from smallest to largest
	{
		for (int j = 0; j < n - 1 - i; j++)
		{
			if (a[j] > a[j + 1])
			{
				temp = a[j];
				a[j] = a[j + 1];
				a[j + 1] = temp;
			}
		}
	}
	for (int k = 0; k < n; k++) //find the position of the first element that is larger than divider
	{
		if (a[k] >= divider)
		{
			return k;
		}
	}
	return n;
}