#include <iostream>
#include <vector>
#include <queue>
#include <set>
#include <tuple>
#include <fstream>
#include <sstream>

using namespace std;

struct State {
    int elevator;
    vector<int> items; // floor number of each item (RTG or Microchip)
    int steps;

    bool operator<(const State& other) const {
        return tie(elevator, items) < tie(other.elevator, other.items);
    }

    bool isValid() const {
        for (size_t i = 0; i < items.size(); i += 2) {
            int chipFloor = items[i];
            int genFloor = items[i + 1];
            if (chipFloor != genFloor) {
                for (size_t j = 1; j < items.size(); j += 2) {
                    if (items[j] == chipFloor) return false;
                }
            }
        }
        return true;
    }

    bool isGoal() const {
        for (int floor : items) {
            if (floor != 3) return false;
        }
        return true;
    }
};

vector<State> getNextStates(const State& state) {
    vector<State> nextStates;
    vector<int> directions = {-1, 1}; // Down and Up
    int currentFloor = state.elevator;

    for (int direction : directions) {
        int newFloor = currentFloor + direction;
        if (newFloor < 0 || newFloor > 3) continue; // Floor out of bounds

        for (size_t i = 0; i < state.items.size(); ++i) {
            for (size_t j = i; j < state.items.size(); ++j) {
                if (state.items[i] == currentFloor && state.items[j] == currentFloor) {
                    State nextState = state;
                    nextState.elevator = newFloor;
                    nextState.items[i] = newFloor;
                    nextState.items[j] = newFloor;
                    nextState.steps++;

                    if (nextState.isValid()) {
                        nextStates.push_back(nextState);
                    }
                }
            }
        }
    }
    return nextStates;
}

int solve(State initialState) {
    queue<State> q;
    set<State> visited;

    q.push(initialState);
    visited.insert(initialState);

    while (!q.empty()) {
        State currentState = q.front();
        q.pop();

        if (currentState.isGoal()) {
            return currentState.steps;
        }

        vector<State> nextStates = getNextStates(currentState);
        for (const State& nextState : nextStates) {
            if (visited.find(nextState) == visited.end()) {
                visited.insert(nextState);
                q.push(nextState);
            }
        }
    }

    return -1; // Should not reach here if there's a valid solution
}

int main() {
    int elevator = 0; // Elevator starts on the first floor
    vector<int> initialItems = {
        0, 0, // Thulium Microchip, Thulium Generator
        0, 1, // Plutonium Microchip, Plutonium Generator
        1, 0, // Strontium Microchip, Strontium Generator
        2, 2, // Promethium Microchip, Promethium Generator
        2, 2  // Ruthenium Microchip, Ruthenium Generator
    };

    State initialState = {elevator, initialItems, 0};
    int result = solve(initialState);

    cout << "Minimum number of steps required: " << result << endl;
    return 0;
}
