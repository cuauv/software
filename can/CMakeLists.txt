add_custom_command(OUTPUT
    ${CMAKE_CURRENT_BINARY_DIR}/gen/ShmRegistry.cpp
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/generate.py ${CMAKE_CURRENT_SOURCE_DIR}/templates
    ${CMAKE_CURRENT_SOURCE_DIR}/config ${CMAKE_CURRENT_BINARY_DIR}/gen
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/templates/ShmRegistry.cpp ${CMAKE_CURRENT_SOURCE_DIR}/config/devices.json
        ${CMAKE_CURRENT_SOURCE_DIR}/config/hardware.json
    )
file(GLOB HOST_SOURCES *.cpp)
link_directories(/home/ian/cuauv/software/link-stage)
add_executable(host 
    "Command.cpp"
    "Culler.cpp"
    "Device.cpp"
    "Group.cpp"
    "Handler.cpp"
    "host.cpp"
    "Manager.cpp"
    "Poller.cpp"
    "Thread.cpp"
    "Registry.cpp"
    "SimpleRegistry.cpp"
    "ShmMessageBuilder.cpp"
    ${CMAKE_CURRENT_BINARY_DIR}/gen/ShmRegistry.cpp
    )
target_include_directories(host PRIVATE ${CMAKE_CURRENT_SOURCE_DIR} /home/ian/cuauv/software)
target_link_libraries(host can_util pthread shm ${CURSES_LIBRARIES})
