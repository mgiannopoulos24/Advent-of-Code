import ballerina/io;
import ballerina/regex;

// Function to check if a passphrase is valid (no duplicate words)
function isValidPassphrase(string passphrase) returns boolean {
    string[] words = regex:split(passphrase, "\\s+");
    map<int> wordCount = {};

    foreach string word in words {
        if (wordCount.hasKey(word)) {
            return false; // Duplicate word found
        }
        wordCount[word] = 1;
    }
    return true; // No duplicates found
}

// Main function to read passphrases from a file and count valid ones
public function main() returns error? {
    string filePath = "input_level_4.txt"; 
    
    // Read the file content as a string
    string content = check io:fileReadString(filePath);
    
    // Split content into passphrases based on newlines
    string[] passphrases = regex:split(content, "\\n");

    int validCount = 0;

    foreach string passphrase in passphrases {
        string trimmedPassphrase = passphrase.trim();
        if (trimmedPassphrase.length() == 0) {
            continue; // Skip empty lines
        }
        if (isValidPassphrase(trimmedPassphrase)) {
            validCount += 1;
        }
    }

    io:println("Number of valid passphrases: " + validCount.toString());
}
