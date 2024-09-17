import ballerina/io;

function knotHash(string inputStr) returns string {
    int[] lengths = [];

    // Convert input string to ASCII codes and append the standard suffix
    foreach byte b in inputStr.toBytes() {
        lengths.push(<int> b);
    }
    int[] suffix = [17, 31, 73, 47, 23];
    // Manually append suffix elements to lengths
    foreach int s in suffix {
        lengths.push(s);
    }

    // Initialize the sparse hash
    int[] sparseHash = [];
    foreach int i in 0 ..< 256 {
        sparseHash.push(i);
    }

    int skipSize = 0;
    int pos = 0;

    // Perform 64 rounds
    foreach int _ in 0 ..< 64 {
        foreach int length in lengths {
            if (length > 1) {
                // Get the sublist to reverse
                int[] sublist = [];
                foreach int i in 0 ..< length {
                    sublist.push(sparseHash[(pos + i) % 256]);
                }
                // Reverse the sublist and assign it back
                sublist = sublist.reverse();
                // Place the reversed sublist back into the sparse hash
                foreach int i in 0 ..< length {
                    sparseHash[(pos + i) % 256] = sublist[i];
                }
            }
            // Update position and skip size
            pos = (pos + length + skipSize) % 256;
            skipSize += 1;
        }
    }

    // Compute the dense hash
    int[] denseHash = [];
    int index = 0;
    while index < 256 {
        int block = sparseHash[index];
        foreach int j in index + 1 ..< index + 16 {
            block ^= sparseHash[j];
        }
        denseHash.push(block);
        index += 16;
    }

    // Convert the dense hash to hexadecimal string
    string hexHash = "";
    foreach int val in denseHash {
        string hex = val.toHexString();
        if hex.length() == 1 {
            hex = "0" + hex;
        }
        hexHash += hex;
    }

    return hexHash;
}

// Convert a hexadecimal string to a binary string with exactly 128 bits
function hexToBin(string hexString) returns string {
    map<string> hexToBinMap = {
        "0": "0000", "1": "0001", "2": "0010", "3": "0011",
        "4": "0100", "5": "0101", "6": "0110", "7": "0111",
        "8": "1000", "9": "1001", "a": "1010", "b": "1011",
        "c": "1100", "d": "1101", "e": "1110", "f": "1111"
    };

    string binaryString = "";
    // Iterate over each character in the hex string
    foreach int i in 0 ..< hexString.length() {
        string hexChar = hexString.substring(i, i + 1).toLowerAscii(); // Get one character
        string binValue = hexToBinMap[hexChar] ?: "0000"; // Convert it to binary using the map
        binaryString += binValue;
    }

    return binaryString;
}

// Count the used squares (i.e., '1's in the binary representation)
function countUsedSquares(string keyString) returns int {
    int totalUsed = 0;

    // Iterate over 128 rows
    foreach int i in 0 ..< 128 {
        string rowInput = keyString + "-" + i.toString();
        string knotHashValue = knotHash(rowInput);  // Compute knot hash for each row
        string binaryRep = hexToBin(knotHashValue); // Convert hex hash to binary

        // Count the number of '1's in binaryRep
        int countOnes = 0;
        foreach var c in binaryRep {
            if c == "1" {
                countOnes += 1;
            }
        }
        totalUsed += countOnes;
    }

    return totalUsed;
}

public function main() {
    string keyString = "jxqlasbh";  // Your input key string
    int totalUsedSquares = countUsedSquares(keyString);
    io:println("Total used squares: " + totalUsedSquares.toString());
}
