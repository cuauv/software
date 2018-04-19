#include <fstream>
#include <iostream>
#include <memory>

#include <lib/json.h>

int main() {
    Json::StreamWriterBuilder builder;
    builder.settings_["commentStyle"] = "None";
    std::unique_ptr<Json::StreamWriter> writer(builder.newStreamWriter());

    Json::Value root;
    std::cin >> root;

    writer->write(root, &std::cout);
    std::cout << std::endl;
}
