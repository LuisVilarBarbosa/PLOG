#include <ctime>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <string>
#include <vector>

#define SIZE 10

using namespace std;

size_t price()
{
	return 1000 + rand();
}

size_t duration() // Duration is multiple of 30
{
	return 30 * (1 + rand() % 4);
}

bool restricted()
{
	if (rand() % 5)
		return false;
	return true;
}

bool displayable_any_times_per_day()
{
	if (rand() % 5)
		return true;
	return false;
}

string day()
{
	vector<string> week = { "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday" };
	return week[rand() % week.size()];
}

string hour()
{
	return to_string(rand() % 24) + "." + (rand() % 2 ? "0" : "5");
}

size_t numVotes()
{
	return rand();
}

size_t generateSeries(ifstream &in, ofstream &out, const size_t maxNumSeries)
{
	string serieName;
	size_t id;
	for (id = 1; getline(in, serieName) && id <= maxNumSeries; id++) {
		if (serieName.empty() || serieName.find("'") != string::npos) {	// Prolog uses "'" to start and end strings (bad idea to have that in a name)
			id--;
			continue;
		}
		out << "series(" << id << ", '" << serieName << "', " << price() << ", " << duration() << ", " << restricted() << ", " << displayable_any_times_per_day() << ").\n";
	}
	out << endl;
	return id - 1;	// last id
}

size_t generateSlots(ofstream &out, const size_t numSeries, const size_t maxNumSlots)
{
	size_t numSlots = 1 + ((rand() % numSeries) % maxNumSlots); // The number of available series is always greater than the number of slots to fill.
	for (size_t id = 1; id <= numSlots; id++)
		out << "slot(" << id << ", '" << day() << "', " << hour() << ").\n";
	out << endl;
	return numSlots;
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

	string maxNumSeriesStr, maxNumPreferencesStr, maxNumSlotsStr;
	cout << "Number of series (-1 to all): ";
	getline(cin, maxNumSeriesStr);
	cout << "Maximum number of preferences per serie (-1 to unlimited): ";
	getline(cin, maxNumPreferencesStr);
	cout << "Maximum number of slots (-1 to unlimited): ";
	getline(cin, maxNumSlotsStr);

	int maxNumSeries, maxNumPreferences, maxNumSlots;
	try {
		// ignores letters after number
		maxNumSeries = stoi(maxNumSeriesStr, NULL, 10);
		maxNumPreferences = stoi(maxNumPreferencesStr, NULL, 10);
		maxNumSlots = stoi(maxNumSlotsStr, NULL, 10);
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
	if (maxNumPreferences == -1)
		maxNumPreferences = UINT_MAX;
	if (maxNumSlots == -1)
		maxNumSlots = UINT_MAX;

	ifstream in("series.txt");
	if (!in.is_open()) { cerr << "Unable to open the input file.\n"; return -1; }
	ofstream out("data.pl");
	if (!out.is_open()) { cerr << "Unable to open the output file.\n"; in.close(); return -1; }

	size_t numSeries = generateSeries(in, out, maxNumSeries);
	in.close();
	size_t numSlots = generateSlots(out, numSeries, maxNumSlots);
	generatePreferences(out, numSeries, numSlots);
	out.close();

	return 0;
}
