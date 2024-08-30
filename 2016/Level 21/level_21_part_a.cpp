#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>
#include <sstream>

using namespace std;

string swap_position(string s, int x, int y) {
    swap(s[x], s[y]);
    return s;
}

string swap_letter(string s, char x, char y) {
    replace(s.begin(), s.end(), x, '#');
    replace(s.begin(), s.end(), y, x);
    replace(s.begin(), s.end(), '#', y);
    return s;
}

string rotate_left(string s, int steps) {
    steps = steps % s.length();
    return s.substr(steps) + s.substr(0, steps);
}

string rotate_right(string s, int steps) {
    steps = steps % s.length();
    return s.substr(s.length() - steps) + s.substr(0, s.length() - steps);
}

string rotate_based_on_position(string s, char x) {
    int index = s.find(x);
    int steps = 1 + index + (index >= 4 ? 1 : 0);
    return rotate_right(s, steps);
}

string reverse_positions(string s, int x, int y) {
    reverse(s.begin() + x, s.begin() + y + 1);
    return s;
}

string move_position(string s, int x, int y) {
    char char_to_move = s[x];
    s.erase(s.begin() + x);
    s.insert(s.begin() + y, char_to_move);
    return s;
}

string scramble_password(string s, const vector<string>& operations) {
    for (const string& operation : operations) {
        vector<string> parts;
        string part;
        istringstream iss(operation);
        while (iss >> part) {
            parts.push_back(part);
        }

        if (parts[0] == "swap" && parts[1] == "position") {
            s = swap_position(s, stoi(parts[2]), stoi(parts[5]));
        } else if (parts[0] == "swap" && parts[1] == "letter") {
            s = swap_letter(s, parts[2][0], parts[5][0]);
        } else if (parts[0] == "rotate" && parts[1] == "left") {
            s = rotate_left(s, stoi(parts[2]));
        } else if (parts[0] == "rotate" && parts[1] == "right") {
            s = rotate_right(s, stoi(parts[2]));
        } else if (parts[0] == "rotate" && parts[1] == "based") {
            s = rotate_based_on_position(s, parts[6][0]);
        } else if (parts[0] == "reverse") {
            s = reverse_positions(s, stoi(parts[2]), stoi(parts[4]));
        } else if (parts[0] == "move") {
            s = move_position(s, stoi(parts[2]), stoi(parts[5]));
        }
    }
    return s;
}

vector<string> read_operations_from_file(const string& filename) {
    ifstream file(filename);
    vector<string> operations;
    string line;
    while (getline(file, line)) {
        operations.push_back(line);
    }
    return operations;
}

int main() {
    string filename = "input_level_21.txt";
    vector<string> operations = read_operations_from_file(filename);
    string password = "abcdefgh";
    string scrambled_password = scramble_password(password, operations);
    cout << "The scrambled password is: " << scrambled_password << endl;
    return 0;
}
