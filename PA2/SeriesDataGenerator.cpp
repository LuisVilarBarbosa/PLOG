#include <algorithm>
#include <ctime>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <string>
#include <vector>

#define SIZE 10

using namespace std;

vector<string> loadSeriesNames(ifstream &in)
{
	vector<string> v;
	string str;
	while (getline(in, str)) {
		if (str.empty() || str.find("'") != string::npos) {	// Prolog uses "'" to start and end strings (bad idea to have that in a name)
			cout << "Ignored the string: " << str << ".\n";
			continue;
		}
		else
			v.push_back(str);
	}
	return v;
}

size_t price()
{
	return 1000 + rand();
}

size_t duration() // Duration is multiple of 30
{
	return 30 * (1 + rand() % 4);
}

string partOfTheDay()
{
	size_t minHour = 0, maxHour = 0;
	while (minHour >= maxHour) {
		minHour = rand() % 24;
		maxHour = rand() % 24;
	}
	return to_string(minHour) + ", " + to_string(maxHour);
}

size_t weekDay()
{
	return 1 + rand() % 7;
}

string schedule()
{
	return to_string(rand() % 24) + ", " + (rand() % 2 ? "0" : "30");
}

size_t numVotes()
{
	return 1 + rand();
}

size_t generateSeries(ofstream &out, const size_t numSeriesAvailable, const size_t maxNumSeries)
{
	size_t id, numSeries = min(numSeriesAvailable, maxNumSeries);
	for (id = 1; id <= numSeries; id++)
		out << "series(" << id << ", " << price() << ", " << duration() << ", " << partOfTheDay() << ").\n";
	out << endl;
	return id - 1;	// last id
}

void generateNoSameDays(ofstream &out, const size_t numSeries)
{
	size_t num = 1 + rand() % (numSeries / 2);

	for (size_t i = 0, random[2]; i < num; i++) {
		for (size_t j = 0; j < 2; j++)
			random[j] = 1 + rand() % numSeries;
		out << "noSameDay(" << random[0] << ", " << random[1] << ").\n";
	}
	out << endl;
}

void generateSeriesNames(ofstream &out, const size_t numSeries, const vector<string> &seriesNames)
{
	for (size_t id = 1; id <= numSeries; id++)
		out << "seriesName(" << id << ", '" << seriesNames[id - 1] << "').\n";
	out << endl;
}

size_t generateSlots(ofstream &out, const size_t numSeries, const size_t numSlots)
{
	size_t myNumSlots = min(numSlots, numSeries - 1); // The number of available series is always greater than the number of slots to fill.
	for (size_t id = 1; id <= myNumSlots; id++)
		out << "slot(" << id << ", " << weekDay() << ", " << schedule() << ").\n";
	out << endl;
	return myNumSlots;
}

void generateSlotsDays(ofstream &out)
{
	vector<string> week = { "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday" };
	for (size_t i = 0; i < week.size(); i++)
		out << "slotDay(" << i + 1 << ", '" << week[i] << "').\n";
	out << endl;
}

void generatePreferences(ofstream &out, const size_t numSeries, const size_t numSlots)
{
	out << "preferences([\n";

	// Output column number
	out << "	%";
	for (size_t i = 1; i < numSlots; i++)
		out << setw(SIZE) << i << ", ";
	out << setw(SIZE) << numSlots << "\n";

	// Output row and row number
	for (size_t i = 1; i <= numSeries; i++) {
		out << "	[";
		for (size_t j = 1; j < numSlots; j++) {
			out << setw(SIZE) << numVotes() << ", ";
		}
		out << setw(SIZE) << numVotes();
		if (i == numSeries)
			out << "]	% " << i << "\n";
		else
			out << "],	% " << i << "\n";
	}

	out << "]).\n";
}

int main()
{
	srand((unsigned)time(NULL));

	string maxNumSeriesStr, maxNumSlotsStr;
	cout << "Number of series (-1 to all): ";
	getline(cin, maxNumSeriesStr);
	cout << "Number of slots (-1 to unlimited): ";
	getline(cin, maxNumSlotsStr);

	int maxNumSeries, numSlots;
	try {
		// ignores letters after number
		maxNumSeries = stoi(maxNumSeriesStr, NULL, 10);
		numSlots = stoi(maxNumSlotsStr, NULL, 10);
	}
	catch (invalid_argument e) {
		cerr << e.what() << endl;
		return -1;
	}
	catch (out_of_range e) {
		cerr << e.what() << endl;
		return -1;
	}
	catch (...) {
		cerr << "Unexpected exception catched.\n";
		return -1;
	}

	if (maxNumSeries == -1)
		maxNumSeries = UINT_MAX;
	if (numSlots == -1)
		numSlots = UINT_MAX;

	ifstream in("series.txt");
	if (!in.is_open()) { cerr << "Unable to open the input file.\n"; return -1; }
	vector<string> series = loadSeriesNames(in);
	in.close();

	ofstream out("data.pl");
	if (!out.is_open()) { cerr << "Unable to open the output file.\n"; in.close(); return -1; }

	size_t numSeries = generateSeries(out, series.size(), maxNumSeries);
	generateNoSameDays(out, numSeries);
	generateSeriesNames(out, numSeries, series);
	size_t realNumSlots = generateSlots(out, numSeries, numSlots);
	generateSlotsDays(out);
	generatePreferences(out, numSeries, realNumSlots);

	out.close();

	return 0;
}
