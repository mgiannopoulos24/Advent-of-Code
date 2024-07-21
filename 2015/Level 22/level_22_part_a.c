#include <stdio.h>
#include <limits.h>
#include <stdbool.h>
#include <stdlib.h>

#define MAX_HP 50
#define MAX_MANA 500
#define BOSS_HP 55
#define BOSS_DAMAGE 8

typedef struct {
    int player_hp;
    int player_mana;
    int boss_hp;
    int shield_duration;
    int poison_duration;
    int recharge_duration;
} GameState;

int minManaSpent = INT_MAX;

bool isGameOver(GameState state) {
    return state.player_hp <= 0 || state.boss_hp <= 0;
}

void applyEffects(GameState *state) {
    if (state->poison_duration > 0) {
        state->boss_hp -= 3;
        state->poison_duration--;
    }
    if (state->recharge_duration > 0) {
        state->player_mana += 101;
        state->recharge_duration--;
    }
    if (state->shield_duration > 0) {
        state->shield_duration--;
    }
}

void simulate(GameState state, int manaSpent) {
    if (isGameOver(state)) {
        if (state.boss_hp <= 0) {
            if (manaSpent < minManaSpent) {
                minManaSpent = manaSpent;
            }
        }
        return;
    }
    
    // If mana spent is already more than the minimum found, return
    if (manaSpent >= minManaSpent) {
        return;
    }
    
    // Apply effects before player's turn
    applyEffects(&state);

    // Player's turn
    // Magic Missile
    if (state.player_mana >= 53) {
        GameState nextState = state;
        nextState.boss_hp -= 4;
        nextState.player_mana -= 53;
        simulate(nextState, manaSpent + 53);
    }

    // Drain
    if (state.player_mana >= 73) {
        GameState nextState = state;
        nextState.boss_hp -= 2;
        nextState.player_hp += 2;
        nextState.player_mana -= 73;
        simulate(nextState, manaSpent + 73);
    }

    // Shield
    if (state.player_mana >= 113 && state.shield_duration == 0) {
        GameState nextState = state;
        nextState.shield_duration = 6;
        nextState.player_mana -= 113;
        simulate(nextState, manaSpent + 113);
    }

    // Poison
    if (state.player_mana >= 173 && state.poison_duration == 0) {
        GameState nextState = state;
        nextState.poison_duration = 6;
        nextState.player_mana -= 173;
        simulate(nextState, manaSpent + 173);
    }

    // Recharge
    if (state.player_mana >= 229 && state.recharge_duration == 0) {
        GameState nextState = state;
        nextState.recharge_duration = 5;
        nextState.player_mana -= 229;
        simulate(nextState, manaSpent + 229);
    }
}

int main() {
    GameState initialState = {50, 500, BOSS_HP, 0, 0, 0};

    simulate(initialState, 0);

    printf("Minimum Mana Spent: %d\n", minManaSpent);

    return 0;
}
