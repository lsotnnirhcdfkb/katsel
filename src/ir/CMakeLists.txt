add_subdirectory(typing)
add_subdirectory(value)

target_sources(katselc PUBLIC unit.cpp function.cpp instruction.cpp register.cpp block.cpp)
