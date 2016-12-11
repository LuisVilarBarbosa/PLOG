#include <ctime>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

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
		return true;
	return false;
}

bool displayable_any_times_per_day()
{
	if (rand() % 5)
		return false;
	return true;
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

void generateSlots(size_t maxNumSeries, size_t maxNumPreferences, size_t maxNumSlots)
{
	ifstream in("series.txt");
	if (!in.is_open()) { cerr << "Unable to open the input file.\n"; return; }
	ofstream out("data.txt");
	if (!out.is_open()) { cerr << "Unable to open the output file.\n"; in.close(); return; }
	string serieName;
	
	size_t id;
	for (id = 1; getline(in, serieName) && id <= maxNumSeries; id++) {
		if (serieName == "") {
			id--;
			continue;
		}

		out << "series(" << id << ", '" << serieName << "', " << price() << ", " << duration() << ", " << restricted() << ", " << displayable_any_times_per_day() << ").\n";

		size_t numPreferences = 1 + (rand() % maxNumPreferences);
		for (size_t j = 1; j <= numPreferences; j++)
			out << "preference(" << id << ", '" << day() << "', " << hour() << ", " << numVotes() << ").\n";
		out << endl;
	}

	size_t numSlots = 1 + ((rand() % (id - 1)) % maxNumSlots); // The number of available series is always greater than the number of slots to fill.
	for (size_t i = 1; i <= numSlots; i++)
		out << "slot('" << day() << "', " << hour() << ").\n";

	in.close();
	out.close();
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
	}
	catch (out_of_range e) {
		cerr << e.what() << endl;
	}
	catch (...) {
		cerr << "Unexpected exception catched.\n";
	}

	if (maxNumSeries == -1)
		maxNumSeries = UINT_MAX;
	if (maxNumPreferences == -1)
		maxNumPreferences = UINT_MAX;
	if (maxNumSlots == -1)
		maxNumSlots = UINT_MAX;

	generateSlots(maxNumSeries, maxNumPreferences, maxNumSlots);

	return 0;
}
