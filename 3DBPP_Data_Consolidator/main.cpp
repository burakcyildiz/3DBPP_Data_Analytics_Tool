#include "common.h"
#include <iostream>



int main()
{
	vector<string> filesToRead;

	ifstream myfile1;
	myfile1.open("filesToRead.txt");

	ofstream consolidatedFiles;
	consolidatedFiles.open("Volume Percentages.txt");

	consolidatedFiles << "Very Small" << "\t" << "Small" << "\t" << "Medium" << "\t" << "Large" << "\t" << "Very Large" << endl;

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
		filesToRead.push_back(v.at(i).at(0).c_str());
	}

	string filename;

	v.clear();
	fields.clear();

	for (int q = 0; q < filesToRead.size(); q++)
	{
		string filename = filesToRead[q];
		cout << "Parsing file " << q + 1 << " of " << filesToRead.size() << endl;

		totalItems = 0;
		vsCt = 0;
		sCt = 0;
		mCt = 0;
		lCt = 0;
		vlCt = 0;

		parseInput(filename);
		
		consolidatedFiles << (vsCt / (double)totalItems)*100 << "\t" << (sCt / (double)totalItems)*100 << "\t" << (mCt / (double)totalItems)*100 << "\t"
			<< (lCt / (double)totalItems)*100 << "\t" << (vlCt / (double)totalItems)*100 << endl;
	}

	filesToRead.clear();
	consolidatedFiles.close();

	return 0;
}