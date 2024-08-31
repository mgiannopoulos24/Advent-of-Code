import ballerina/io;
import ballerina/regex;

// Function to check if two words are anagrams of each other
function areAnagrams(string word1, string word2) returns boolean {
    if (word1.length() != word2.length()) {
        return false;
    }
    // Convert words to arrays of characters, sort them, and then compare
    string sortedWord1 = sortCharacters(word1);
    string sortedWord2 = sortCharacters(word2);
    return sortedWord1 == sortedWord2;
}

// Helper function to sort the characters of a string
function sortCharacters(string word) returns string {
    string[] characters = regex:split(word, ""); // Corrected the split function usage
    string[] sortedCharacters = characters.sort(); // Assign sorted array to a new variable
    return arrayToString(sortedCharacters); // Join them back into a single string
}

// Helper function to concatenate an array of strings into a single string
function arrayToString(string[] array) returns string {
    string result = "";
    foreach string s in array {
        result += s;
    }
    return result;
}

// Function to check if a passphrase is valid (no duplicate words and no anagrams)
function isValidPassphrase(string passphrase) returns boolean {
    string[] words = regex:split(passphrase, "\\s+"); // Split passphrase into words
    map<int> wordCount = {};
    map<boolean> sortedWordSet = {}; // Use a map to track unique sorted words

    foreach string word in words {
        if (wordCount.hasKey(word)) {
            return false; // Duplicate word found
        }
        wordCount[word] = 1;

        // Check for anagrams
        string sortedWord = sortCharacters(word);
        if (sortedWordSet.hasKey(sortedWord)) {
            return false; // Anagram found
        }
        sortedWordSet[sortedWord] = true;
    }
    return true; // No duplicates or anagrams found
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
