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

// Function to compute total sizes and collect directories
function computeSizesAndCollect(directory, allDirs) {
    let total = 0;
    for (const child of directory.children) {
        if (child.type === 'file') {
            total += child.size;
        } else if (child.type === 'directory') {
            total += computeSizesAndCollect(child, allDirs);
        }
    }
    directory.totalSize = total;
    allDirs.push(total); // Collect the size of this directory
    return total;
}

// Main function
function main() {
    const inputPath = path.join(__dirname, 'input_level_7.txt');
    const input = fs.readFileSync(inputPath, 'utf-8');
    const lines = input.split('\n');

    // Build the filesystem tree
    const root = buildFileSystem(lines);

    // Compute sizes and collect all directories' sizes
    const allDirs = [];
    const usedSpace = computeSizesAndCollect(root, allDirs);

    // -------------------- Part Two --------------------
    const totalDiskSpace = 70000000;
    const requiredUnusedSpace = 30000000;
    const currentUnusedSpace = totalDiskSpace - usedSpace;
    const additionalSpaceNeeded = requiredUnusedSpace - currentUnusedSpace;

    console.log(`\n--- Part Two ---`);
    console.log(`Total Disk Space: ${totalDiskSpace}`);
    console.log(`Used Space: ${usedSpace}`);
    console.log(`Current Unused Space: ${currentUnusedSpace}`);
    console.log(`Additional Space Needed: ${additionalSpaceNeeded}`);

    if (additionalSpaceNeeded <= 0) {
        console.log(`No need to delete any directory. Enough space is already available.`);
    } else {
        // Find the smallest directory that, if deleted, would free up enough space
        const candidateDirs = allDirs.filter(size => size >= additionalSpaceNeeded);
        if (candidateDirs.length === 0) {
            console.log(`No directory is large enough to free up the required space.`);
        } else {
            const smallestDirSize = Math.min(...candidateDirs);
            console.log(`Smallest directory size that can be deleted to free up at least ${additionalSpaceNeeded}: ${smallestDirSize}`);
        }
    }
}

// Run the main function
main();
