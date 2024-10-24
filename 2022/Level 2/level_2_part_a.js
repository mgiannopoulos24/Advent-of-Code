const fs = require('fs');
const path = require('path');

// Path to the input file
const inputPath = path.join(__dirname, 'input_level_2.txt');

// Mappings for opponent's moves
const opponentMoves = {
    'A': 'Rock',
    'B': 'Paper',
    'C': 'Scissors'
};

// Mappings for your moves
const yourMoves = {
    'X': 'Rock',
    'Y': 'Paper',
    'Z': 'Scissors'
};

// Scores for each shape
const shapeScores = {
    'Rock': 1,
    'Paper': 2,
    'Scissors': 3
};

// Function to determine the outcome of a round
function determineOutcome(opponent, you) {
    if (opponent === you) {
        return 'Draw';
    }

    if (
        (you === 'Rock' && opponent === 'Scissors') ||
        (you === 'Paper' && opponent === 'Rock') ||
        (you === 'Scissors' && opponent === 'Paper')
    ) {
        return 'Win';
    } else {
        return 'Loss';
    }
}

// Function to get outcome score
function getOutcomeScore(outcome) {
    switch (outcome) {
        case 'Win':
            return 6;
        case 'Draw':
            return 3;
        case 'Loss':
            return 0;
        default:
            return 0;
    }
}

// Read the input file
fs.readFile(inputPath, 'utf8', (err, data) => {
    if (err) {
        console.error('Error reading the input file:', err);
        return;
    }

    // Split the input into lines
    const lines = data.split(/\r?\n/).filter(line => line.trim() !== '');

    let totalScore = 0;

    lines.forEach((line, index) => {
        const [opponentCode, yourCode] = line.trim().split(' ');

        const opponentMove = opponentMoves[opponentCode];
        const yourMove = yourMoves[yourCode];

        if (!opponentMove || !yourMove) {
            console.warn(`Invalid moves on line ${index + 1}: "${line}". Skipping this line.`);
            return;
        }

        const outcome = determineOutcome(opponentMove, yourMove);
        const shapeScore = shapeScores[yourMove];
        const outcomeScore = getOutcomeScore(outcome);
        const roundScore = shapeScore + outcomeScore;

        // Debugging logs (optional)
        // console.log(`Round ${index + 1}: Opponent chose ${opponentMove}, you chose ${yourMove}. Outcome: ${outcome}. Score: ${roundScore}`);

        totalScore += roundScore;
    });

    console.log('Your total score is:', totalScore, 'points.');
});
