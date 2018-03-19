#pragma once

#include <vector>
#include <fstream>
#include <string>
#include <algorithm>
#include <ctime>

using namespace std;

int totalItems;
int vsCt;
int sCt;
int mCt;
int lCt;
int vlCt;

template <typename Container>
Container& split(Container& result,
	const typename Container::value_type& s,
	const typename Container::value_type& delimiters)
{
	result.clear();
	size_t current;
	size_t next = -1;
	do {
		current = next + 1;
		next = s.find_first_of(delimiters, current);
		result.push_back(s.substr(current, next - current));
	} while (next != Container::value_type::npos);
	return result;
}

void parseInput(string fileName)
{
	ifstream myfile1;
	myfile1.open(fileName);

	string line;

	size_t pos = 0;
	string token;
	int tempTime;

	const string delimiter = "\t";
	vector<vector<string>> v;
	vector<string> fields;

	//Read data
	for (myfile1; getline(myfile1, line);)
	{
		v.push_back(split(fields, line, delimiter));
	}
	myfile1.close();

	for (int i = 0; i < v.size(); i++)
	{
		int repetition = atoi(v.at(i).at(15).c_str());

		for (int j = 0; j < repetition; j++)
		{
			totalItems++;
			double vol = atoi(v.at(i).at(0).c_str()) * atoi(v.at(i).at(1).c_str()) * atoi(v.at(i).at(2).c_str()) / 1000000;

			if (vol <= 12.04)
			{
				vsCt++;
			}
			else if (vol <= 20.24)
			{
				sCt++;
			}
			else if (vol <= 32.42)
			{
				mCt++;
			}
			else if (vol <= 54.08)
			{
				lCt++;
			}
			else
			{
				vlCt++;
			}
			//outputfile << atoi(v.at(i).at(0).c_str()) << "\t" << atoi(v.at(i).at(1).c_str()) << "\t" << atoi(v.at(i).at(2).c_str()) << endl;
		}
	}
	fields.clear();
	v.clear();
}