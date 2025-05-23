const fs = require('fs');

class BlizzardBasin {
    constructor(mapInput) {
        this.map = mapInput.split('\n').filter(line => line.trim() !== '');
        this.height = this.map.length;
        this.width = this.map[0].length;
        this.blizzards = this.parseBlizzards();
        this.start = this.findStart();
        this.end = this.findEnd();
    }

    parseBlizzards() {
        const blizzards = [];
        for (let y = 0; y < this.height; y++) {
            for (let x = 0; x < this.width; x++) {
                const char = this.map[y][x];
                if ('^v<>'.includes(char)) {
                    blizzards.push({ x, y, dir: char });
                }
            }
        }
        return blizzards;
    }

    findStart() {
        return { x: this.map[0].indexOf('.'), y: 0 };
    }

    findEnd() {
        return { x: this.map[this.height - 1].indexOf('.'), y: this.height - 1 };
    }

    moveBlizzards(blizzards) {
        const newBlizzards = blizzards.map(b => {
            let { x, y } = b;
            switch (b.dir) {
                case '^': y = y === 1 ? this.height - 2 : y - 1; break;
                case 'v': y = y === this.height - 2 ? 1 : y + 1; break;
                case '<': x = x === 1 ? this.width - 2 : x - 1; break;
                case '>': x = x === this.width - 2 ? 1 : x + 1; break;
            }
            return { x, y, dir: b.dir };
        });
        return newBlizzards;
    }

    isValidMove(x, y, blizzards) {
        // Check wall boundaries
        if (y < 0 || y >= this.height || x < 0 || x >= this.width) return false;
        
        // Check if it's the start or end
        if ((x === this.start.x && y === this.start.y) || 
            (x === this.end.x && y === this.end.y)) return true;
        
        // Check if it's a wall
        if (this.map[y][x] === '#') return false;
        
        // Check if it's occupied by a blizzard
        return !blizzards.some(b => b.x === x && b.y === y);
    }

    findShortestPath(startPos, endPos) {
        const seen = new Set();
        const queue = [{ ...startPos, time: 0, blizzards: this.blizzards }];
        
        while (queue.length > 0) {
            const { x, y, time, blizzards } = queue.shift();
            
            // Reached the end
            if (x === endPos.x && y === endPos.y) return time;
            
            // Create a state key for tracking seen states
            const stateKey = `${x},${y},${time % (this.width * this.height)}`;
            if (seen.has(stateKey)) continue;
            seen.add(stateKey);
            
            // Move blizzards for next state
            const nextBlizzards = this.moveBlizzards(blizzards);
            
            // Try all possible moves (including waiting)
            const moves = [
                { dx: 0, dy: 0 },   // wait
                { dx: 0, dy: -1 },  // up
                { dx: 0, dy: 1 },   // down
                { dx: -1, dy: 0 },  // left
                { dx: 1, dy: 0 }    // right
            ];
            
            for (const move of moves) {
                const newX = x + move.dx;
                const newY = y + move.dy;
                
                if (this.isValidMove(newX, newY, nextBlizzards)) {
                    queue.push({
                        x: newX,
                        y: newY,
                        time: time + 1,
                        blizzards: nextBlizzards
                    });
                }
            }
            
            // Sort queue by time to prioritize quicker paths
            queue.sort((a, b) => a.time - b.time);
        }
        
        return -1; // No path found
    }

    solvePartOne() {
        return this.findShortestPath(this.start, this.end);
    }

}

// Read input from file
const input = fs.readFileSync('input_level_24.txt', 'utf8');
const basin = new BlizzardBasin(input);

console.log('Fewest number of minutes required to avoid the blizzards and reach the goal:', basin.solvePartOne());