#include <stdio.h>
#include <stdbool.h>
#include <limits.h>

typedef struct {
    int hit_points;
    int mana;
    int armor;
    int boss_hit_points;
    int boss_damage;
    int shield_timer;
    int poison_timer;
    int recharge_timer;
} GameState;

void apply_effects(GameState *state) {
    if (state->shield_timer > 0) {
        state->armor = 7;
        state->shield_timer--;
    } else {
        state->armor = 0;
    }

    if (state->poison_timer > 0) {
        state->boss_hit_points -= 3;
        state->poison_timer--;
    }

    if (state->recharge_timer > 0) {
        state->mana += 101;
        state->recharge_timer--;
    }
}

bool player_turn(GameState *state, int spell_cost, int damage, int heal, int shield, int poison, int recharge) {
    if (state->mana < spell_cost) return false;

    state->mana -= spell_cost;
    state->boss_hit_points -= damage;
    state->hit_points += heal;

    if (shield) state->shield_timer = 6;
    if (poison) state->poison_timer = 6;
    if (recharge) state->recharge_timer = 5;

    return true;
}

void boss_turn(GameState *state) {
    int damage = state->boss_damage - state->armor;
    if (damage < 1) damage = 1;
    state->hit_points -= damage;
}

bool simulate(GameState state, int *min_mana_spent, int mana_spent) {
    if (mana_spent >= *min_mana_spent) return false;
    if (state.boss_hit_points <= 0) {
        *min_mana_spent = mana_spent;
        return true;
    }
    if (state.hit_points <= 0) return false;

    apply_effects(&state);
    if (state.boss_hit_points <= 0) {
        *min_mana_spent = mana_spent;
        return true;
    }

    GameState new_state = state;
    boss_turn(&new_state);
    if (new_state.hit_points <= 0) return false;

    // Try all possible spells
    if (player_turn(&state, 53, 4, 0, 0, 0, 0) && simulate(state, min_mana_spent, mana_spent + 53)) return true;
    if (player_turn(&state, 73, 2, 2, 0, 0, 0) && simulate(state, min_mana_spent, mana_spent + 73)) return true;
    if (player_turn(&state, 113, 0, 0, 1, 0, 0) && simulate(state, min_mana_spent, mana_spent + 113)) return true;
    if (player_turn(&state, 173, 0, 0, 0, 1, 0) && simulate(state, min_mana_spent, mana_spent + 173)) return true;
    if (player_turn(&state, 229, 0, 0, 0, 0, 1) && simulate(state, min_mana_spent, mana_spent + 229)) return true;

    return false;
}

int main() {
    GameState initial_state = {50, 500, 0, 71, 10, 0, 0, 0};
    int min_mana_spent = INT_MAX;

    simulate(initial_state, &min_mana_spent, 0);

    printf("Minimum mana spent: %d\n", min_mana_spent);
    return 0;
}
