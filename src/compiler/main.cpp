#include <iostream>
#include <fstream>

std::string readFile(char *filename) {
    std::ifstream filein;
    filein.open(filename);

    if (filein.is_open()) {
        std::string contents;

        filein.seekg(0, std::ios::end);
        contents.resize(filein.tellg());

        filein.seekg(0, std::ios::beg);
        filein.read(&contents[0], contents.size());

        filein.close();
    
        return contents;
    } else {
        std::cout << "Could not open file" << std::endl;
        return nullptr;
    }

}

void compileFile(char *filename) {
    std::string contents (readFile(filename));
}

int main(int argc, char *argv[]) {
    if (argc == 2) {
        // Compile file
        compileFile(argv[1]);
    } else {
        std::cout << "Usage: ctrbelc <file>\n"
            "\n"
            "file - the main file to compile\n" << std::endl;
    }

    return 0;
}
