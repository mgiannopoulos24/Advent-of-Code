import ballerina/io;
import ballerina/regex;

type Program record {
    string name;
    int weight;
    string[] supports;
};

function parseLine(string line) returns Program|error {
    string[] parts = regex:split(line, " -> ");
    string[] programInfo = regex:split(parts[0], " ");
    string name = programInfo[0];
    int weight = check 'int:fromString(programInfo[1].substring(1, programInfo[1].length() - 1));
    string[] supports = parts.length() > 1 ? regex:split(parts[1], ", ") : [];
    return {name: name, weight: weight, supports: supports};
}

function buildProgramsAndSupports(string[] lines) returns map<Program>|error {
    map<Program> programs = {};
    map<boolean> supportedBy = {};

    foreach var line in lines {
        Program|error programResult = parseLine(line);
        if (programResult is error) {
            return programResult;
        }
        Program program = programResult;
        programs[program.name] = program;
        foreach var support in program.supports {
            supportedBy[support] = true;
        }
    }

    return programs;
}

function findBottomProgram(map<Program> programs) returns string {
    map<boolean> supportedPrograms = {};

    // Collect all supported programs
    foreach var program in programs {
        foreach var support in program.supports {
            supportedPrograms[support] = true;
        }
    }

    // Find the program that is not supported by any other program
    foreach var program in programs {
        if (!supportedPrograms.hasKey(program.name)) {
            return program.name;
        }
    }
    return "";
}

public function main() returns error? {
    // Read the input file
    string fileContent = check io:fileReadString("input_level_7.txt");
    string[] lines = regex:split(fileContent, "\n");

    // Build the programs and supports
    map<Program>|error programsResult = buildProgramsAndSupports(lines);
    if (programsResult is error) {
        return programsResult;
    }
    map<Program> programs = programsResult;

    // Find the bottom program
    string bottomProgram = findBottomProgram(programs);

    // Output the result
    io:println("The bottom program is: ", bottomProgram);
}