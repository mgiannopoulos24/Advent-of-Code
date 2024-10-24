const fs = require('fs');
const path = require('path');

// Define classes for File and Directory
class File {
    constructor(name, size) {
        this.name = name;
        this.size = size;
        this.type = 'file';
    }
}

class Directory {
    constructor(name, parent = null) {
        this.name = name;
        this.children = []; // Can contain File or Directory objects
        this.parent = parent; // Reference to parent Directory
        this.type = 'directory';
        this.totalSize = 0; // To be computed later
    }

    addChild(child) {
        this.children.push(child);
    }

    // Find a child directory by name
    getChildDirectory(name) {
        return this.children.find(child => child.type === 'directory' && child.name === name);
    }
}

// Function to parse the input and build the filesystem tree
function buildFileSystem(lines) {
    const root = new Directory('/');
    let currentDir = root;

    for (let i = 0; i < lines.length; i++) {
        const line = lines[i].trim();
        if (line.startsWith('$')) {
            const parts = line.split(' ');
            const command = parts[1];

            if (command === 'cd') {
                const arg = parts[2];
                if (arg === '/') {
                    currentDir = root;
                } else if (arg === '..') {
                    if (currentDir.parent !== null) {
                        currentDir = currentDir.parent;
                    }
                } else {
                    const nextDir = currentDir.getChildDirectory(arg);
                    if (nextDir) {
                        currentDir = nextDir;
                    } else {
                        // If directory does not exist, create it
                        const newDir = new Directory(arg, currentDir);
                        currentDir.addChild(newDir);
                        currentDir = newDir;
                    }
                }
            } else if (command === 'ls') {
                // Read the following lines until the next command or end of input
                i++;
                while (i < lines.length && !lines[i].startsWith('$')) {
                    const entry = lines[i].trim();
                    if (entry.startsWith('dir')) {
                        const dirName = entry.split(' ')[1];
                        // Check if directory already exists
                        if (!currentDir.getChildDirectory(dirName)) {
                            const newDir = new Directory(dirName, currentDir);
                            currentDir.addChild(newDir);
                        }
                    } else {
                        const [size, fileName] = entry.split(' ');
                        const file = new File(fileName, parseInt(size, 10));
                        currentDir.addChild(file);
                    }
                    i++;
                }
                i--; // Adjust for the outer loop increment
            }
        }
    }

    return root;
}

// Function to compute total sizes and collect directories with size <= 100000
function computeSizesAndCollect(directory, limit, qualifyingDirs) {
    let total = 0;
    for (const child of directory.children) {
        if (child.type === 'file') {
            total += child.size;
        } else if (child.type === 'directory') {
            total += computeSizesAndCollect(child, limit, qualifyingDirs);
        }
    }
    directory.totalSize = total;
    if (total <= limit) {
        qualifyingDirs.push(total);
    }
    return total;
}

// Main function
function main() {
    const inputPath = path.join(__dirname, 'input_level_7.txt');
    const input = fs.readFileSync(inputPath, 'utf-8');
    const lines = input.split('\n');

    // Build the filesystem tree
    const root = buildFileSystem(lines);

    // Compute sizes and collect qualifying directories
    const qualifyingDirs = [];
    computeSizesAndCollect(root, 100000, qualifyingDirs);

    // Sum the qualifying directory sizes
    const sum = qualifyingDirs.reduce((acc, size) => acc + size, 0);
    console.log(`Sum of total sizes of qualifying directories: ${sum}`);
}

// Run the main function
main();
