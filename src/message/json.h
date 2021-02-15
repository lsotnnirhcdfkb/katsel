#pragma once

#include <variant>
#include <sstream>
#include <string>
#include <map>

namespace JSON {
    class String; class Number; class Object; class Array; class Bool;
    using Value = std::variant<String, Number, Object, Array, Bool>;

    class String {
    public:
        std::string value;
        inline std::string stringify() const;
    };
    class Number {
    public:
        double value;
        inline std::string stringify() const;
    };
    class Object {
    public:
        std::map<std::string, Value> values;
        inline std::string stringify() const;
    };
    class Array {
    public:
        std::vector<Value> values;
        inline std::string stringify() const;
    };
    class Bool {
    public:
        bool value;
        inline std::string stringify() const;
    };

    inline std::string stringify(Value const &value) {
        return std::visit([] (auto &&val) { return val.stringify(); }, value);
    }

    std::string String::stringify() const {
        std::stringstream ss;
        // TODO: do escapes
        ss << '"' << value << '"';
        return ss.str();
    }
    std::string Number::stringify() const {
        return std::to_string(value);
    }
    std::string Object::stringify() const {
            std::stringstream ss;
            ss << '{';
            bool first = true;
            for (auto const &item : values) {
                if (!first)
                    ss << ", ";
                ss << '"' << item.first << "\": " << ::JSON::stringify(item.second);

                first = false;
            }
            ss << '}';
            return ss.str();
        }
    std::string Array::stringify() const {
            std::stringstream ss;
            ss << '[';
            bool first = true;
            for (auto const &item : values) {
                if (!first)
                    ss << ", ";
                ss << ::JSON::stringify(item);

                first = false;
            }
            ss << ']';
            return ss.str();
        }
    std::string Bool::stringify() const {
        return std::to_string(value);
    }
}
