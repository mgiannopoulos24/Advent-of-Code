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

// Mappings for desired outcomes
const desiredOutcomes = {
    'X': 'Lose',
    'Y': 'Draw',
    'Z': 'Win'
};

// Scores for each shape
const shapeScores = {
    'Rock': 1,
    'Paper': 2,
    'Scissors': 3
};

// Scores for each outcome
const outcomeScores = {
    'Lose': 0,
    'Draw': 3,
    'Win': 6
};

// Function to determine the shape you need to play based on desired outcome
function determineYourMove(opponent, desiredOutcome) {
    switch (desiredOutcome) {
        case 'Draw':
            return opponent;
        case 'Win':
            if (opponent === 'Rock') return 'Paper';
            if (opponent === 'Paper') return 'Scissors';
            if (opponent === 'Scissors') return 'Rock';
        case 'Lose':
            if (opponent === 'Rock') return 'Scissors';
            if (opponent === 'Paper') return 'Rock';
            if (opponent === 'Scissors') return 'Paper';
        default:
            return null;
    }
}

// Read the input file
fs.readFile(inputPath, 'utf8', (err, data) => {
    if (err) {
        console.error('Error reading the input file:', err);
        return;
    }

    // Split the input into lines and filter out any empty lines
    const lines = data.split(/\r?\n/).filter(line => line.trim() !== '');

    let totalScore = 0;

    lines.forEach((line, index) => {
        const [opponentCode, desiredOutcomeCode] = line.trim().split(' ');

        const opponentMove = opponentMoves[opponentCode];
        const desiredOutcome = desiredOutcomes[desiredOutcomeCode];

        if (!opponentMove || !desiredOutcome) {
            console.warn(`Invalid data on line ${index + 1}: "${line}". Skipping this line.`);
            return;
        }

        const yourMove = determineYourMove(opponentMove, desiredOutcome);

        if (!yourMove) {
            console.warn(`Could not determine your move for line ${index + 1}: "${line}". Skipping this line.`);
            return;
        }

        const shapeScore = shapeScores[yourMove];
        const outcomeScore = outcomeScores[desiredOutcome];
        const roundScore = shapeScore + outcomeScore;

        // Debugging logs (optional)
        // console.log(`Round ${index + 1}: Opponent chose ${opponentMove}, desired outcome is ${desiredOutcome}. You chose ${yourMove}. Score: ${roundScore}`);

        totalScore += roundScore;
    });

    console.log('Your total score is:', totalScore, 'points.');
});
