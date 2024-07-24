#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

typedef struct {
    int hp;
    int mana;
    int armor;
} Player;

typedef struct {
    int hp;
    int damage;
} Boss;

typedef struct {
    int shield_timer;
    int poison_timer;
    int recharge_timer;
} Effects;

void apply_effects(Player *player, Boss *boss, Effects *effects) {
    if (effects->shield_timer > 0) {
        player->armor = 7;
        effects->shield_timer--;
    } else {
        player->armor = 0;
    }
    if (effects->poison_timer > 0) {
        boss->hp -= 3;
        effects->poison_timer--;
    }
    if (effects->recharge_timer > 0) {
        player->mana += 101;
        effects->recharge_timer--;
    }
}

int player_turn(Player player, Boss boss, Effects effects, int mana_spent);

int boss_turn(Player player, Boss boss, Effects effects, int mana_spent) {
    apply_effects(&player, &boss, &effects);

    if (boss.hp <= 0) return mana_spent;  // Player wins

    // Boss attacks
    int damage = boss.damage - player.armor;
    if (damage < 1) damage = 1;
    player.hp -= damage;

    if (player.hp <= 0) return INT_MAX;  // Player loses

    return player_turn(player, boss, effects, mana_spent);
}

int player_turn(Player player, Boss boss, Effects effects, int mana_spent) {
    // Lose 1 hit point at the start of the player's turn (hard mode rule)
    player.hp -= 1;
    if (player.hp <= 0) return INT_MAX;  // Player loses

    apply_effects(&player, &boss, &effects);

    if (boss.hp <= 0) return mana_spent;  // Player wins

    int min_mana_spent = INT_MAX;

    // Magic Missile
    if (player.mana >= 53) {
        Player new_player = player;
        Boss new_boss = boss;
        Effects new_effects = effects;
        new_player.mana -= 53;
        new_boss.hp -= 4;
        min_mana_spent = boss_turn(new_player, new_boss, new_effects, mana_spent + 53);
    }

    // Drain
    if (player.mana >= 73) {
        Player new_player = player;
        Boss new_boss = boss;
        Effects new_effects = effects;
        new_player.mana -= 73;
        new_player.hp += 2;
        new_boss.hp -= 2;
        int result = boss_turn(new_player, new_boss, new_effects, mana_spent + 73);
        if (result < min_mana_spent) min_mana_spent = result;
    }

    // Shield
    if (player.mana >= 113 && effects.shield_timer == 0) {
        Player new_player = player;
        Boss new_boss = boss;
        Effects new_effects = effects;
        new_player.mana -= 113;
        new_effects.shield_timer = 6;
        int result = boss_turn(new_player, new_boss, new_effects, mana_spent + 113);
        if (result < min_mana_spent) min_mana_spent = result;
    }

    // Poison
    if (player.mana >= 173 && effects.poison_timer == 0) {
        Player new_player = player;
        Boss new_boss = boss;
        Effects new_effects = effects;
        new_player.mana -= 173;
        new_effects.poison_timer = 6;
        int result = boss_turn(new_player, new_boss, new_effects, mana_spent + 173);
        if (result < min_mana_spent) min_mana_spent = result;
    }

    // Recharge
    if (player.mana >= 229 && effects.recharge_timer == 0) {
        Player new_player = player;
        Boss new_boss = boss;
        Effects new_effects = effects;
        new_player.mana -= 229;
        new_effects.recharge_timer = 5;
        int result = boss_turn(new_player, new_boss, new_effects, mana_spent + 229);
        if (result < min_mana_spent) min_mana_spent = result;
    }

    return min_mana_spent;
}

int main() {
    Player player = {50, 500, 0};
    Boss boss = {71, 10}; // Change these stats according to your input
    Effects effects = {0, 0, 0};

    int result = player_turn(player, boss, effects, 0);
    printf("Minimum mana spent: %d\n", result);

    return 0;
}
