##########################    浮骚部分   ####################################

cmake_minimum_required(VERSION 3.13)

project(Caffe)
# 这句命令执行后，自动产生了以下变量(部分):
# PROJECT_NAME ， 值为 Caffe
# PROJECT_SOURCE_DIR, CMakeLists.txt所在目录  # 重点
# PROJECT_BINARY_DIR, 执行cmake命令时所在的目录 # 重点

set(SOURCES
    src/Hello.cpp
	src/main.cpp
)

# set the C++ standard to C++ 11
set(CMAKE_CXX_STANDARD 11) # on all targets

# $ENV{HOME}
set(CMAKE_CXX_FLAGS_DEBUG "$ENV{CXXFLAGS} -O0 -Wall -g -ggdb -DDEBUG")
set(CMAKE_CXX_FLAGS_RELEASE "$ENV{CXXFLAGS} -O3 -pipe -Wall")

set(CMAKE_CXX_COMPILER g++)

# 指定输出目录
set(LIBRARY_OUTPUT_PATH ${CMAKE_BINARY_DIR}/lib)
set(EXECUTABLE_OUTPUT_PATH ${CMAKE_BINARY_DIR}/bin)


# execute_process 执行shell命令
execute_process(
    COMMAND git log -1 --format=%H
    WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
    OUTPUT_VARIABLE GIT_VERSION
)

string(REGEX REPLACE "[\n\t\r]" "" GIT_VERSION ${GIT_VERSION})

message(STATUS "hello, dir is " ${DIR})

# 7， add_definitions 设置编译器参数，-D可向代码中添加宏定义
add_definitions("-DLINUX -enable-threads=posix -pipe")
add_definitions(-DHAVE_HYPERSCAN=123)  # 相当于代码里  #define HAVE_HYPERSCAN 123
add_definitions(-std=c++11)  # 此语句用作添加编译器参数时，作用于所有编译器


# 变量追加定义
list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake/Modules)
# 在 CMAKE_MODULE_PATH 变量后面追加元素

# 包含文件或模块
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)

# include可以实现cmake函数/宏的复用
include(${CMAKE_MODULE_PATH}/utils.cmake)
include(ExternalProject/CMakeLists.txt) # 在 CMAKE_MODULE_PATH对应的目录中或
# CMake安装目录的Modules目录下(默认/usr/share/cmake/Modules)
# 搜索对应的xxx.cmake文件。此时不需要加.cmake后缀
include(CheckCCompilerFlag RESULT_VARIABLE search_result) # 变量会存放文件绝对路径或"NOTFOUND"



# 指定编译器参数
set(CMAKE_CXX_COMPILE_FEATURES -Wall -std=c++11 -w)  # -w关闭警告
add_compile_options(-std=c++11)
add_definitions(-fpermissive)   # 模板错误降低为警告

# 关闭访问控制
add_definitions("-fno-access-control")

# 自定义头文件包含目录
include_directories(
    ${PROJECT_BINARY_DIR}/inc
    ${CMAKE_CURRENT_LIST_DIR}/inc/3rd/dpdk
    ${CMAKE_CURRENT_LIST_DIR}/inc/packages
)

# 加载子目录中的CMakeList.txt文件并执行 （往往包含targe）
add_subdirectory(src/gtest)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/packages/vty/vtygnu)
# vty/vtygnu目录下有一个CMakeLists.txt, 其中有如下内容：
# message(STATUS "vtygnu CMakeLists.txt is in dir " ${CMAKE_CURRENT_LIST_DIR})
# set(CMAKE_C_COMPILER gcc)
# aux_source_directory(${CMAKE_CURRENT_LIST_DIR} DIR_LIB_VTYGNU)
# include_directories(${PROJECT_SOURCE_DIR}/include/3rdinc/vty)
# add_library (vtygnu ${DIR_LIB_VTYGNU})
# 之后这个vtygnu就可以直接在此文件中使用

# 指定依赖关系
add_dependencies(pytest pycaffe) # pycaffe目标完成后再执行pytest目标
# 一般由一下三种之一创建
add_custom_target(mylib DEPENDS ${MYLIB})
add_executable(${APPNAME} ${SRC_DIR_LISTS})
add_library(mylib2 STATIC ${MYLIB2})


# 自定义链接器查找库的路径
link_directories(
    ${LIBRARY_OUTPUT_PATH}/lib`
    ${DPDKLIB_DIRS}
    ${CMAKE_CURRENT_LIST_DIR}/3rdlib/hyperscan
)
# 这样，hyperscan目录下的 libhs.a, libhs_runtime.a就可以直接在此文件中使用。

# link_libraries(hs hs_runtime)

# 查找指定目录中所有源文件的名称，并自定义变量存储之。
aux_source_directory(./src SRC_DIR)

#####################################################################
#  create a static library
#####################################################################
add_library(hello_library STATIC
    src/Hello.cpp
)
target_include_directories(hello_library
	PUBLIC
		${PROJECT_SOURCE_DIR}/include
)
# add an executable with the above sources
add_executable(hello_binary
	src/main.cpp
)
# link the new hello_library target with the hello_binary target
target_link_libraries(hello_binary
	PRIVATE
		hello_library
)
#######################################################################

#####################################
# 动态库
#####################################
add_library(hello_library SHARED
	src/Hello.cpp
)

add_library(hello::library ALIAS hello_library)

target_include_directories(hello_library
	PUBLIC
		${PROJECT_SOURCE_DIR}/include	
)

add_executable(hello_binary
	src/main.cpp
)

target_link_libraries(hello_binary
	PRIVATE
		hello::library
)

target_compile_options(hello_binary
	PRIVATE
		-fPIC
)

##################################### 动态库 END  #########################



# add_executable 添加可执行文件
add_executable(Hello Hello.c File2.c File3.c File4.c)

# 4， 设置target的编译时包含路径
#     when running g++ these will be include as -I/directory/path/
target_include_directories(hello_headers
	PRIVATE
		${PROJECT_SOURCE_DIR}/include
)

# build
aux_source_directory(${CMAKE_CURRENT_LIST_DIR}/app/GnXDR/src DIR_APP_GNXDR)

# 将给定的库链接到一个目标上
target_link_libraries(GnXDR vtycmd vtygnu hs hs_runtime)
target_link_libraries(GnXDR "-W1,--start-group", middle serialize "-W1,--end-group")

# 命令行参数控制
option(GTEST "my description" OFF) # 默认关闭
if(GTEST)
    add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/gtest)
    add_definitions(-DGTEST)
    message(STATUS "GTEST ON")
endif(GTEST)

# 在命令行可控制此开关
# `$ cmake .. -DGEST=ON`


set(HELLO_SRCS Hello.c File.c File2.c)
# 条件判断
if (WIN32)
    set(HELLO_SRCS ${HELLO_SRCS} WinSupport.c)
else()
    set(HELLO_SRCS ${HELLO_SRCS} UnixSupport.c)
endif()

find_library(TCL_LIBRARY
    NAMES tcl tcl83 tcl84 tcl82
    PATHS /usr/lib /usr/local/lib
)

if(TCL_LIBRARY)
    target_link_libraries(Hello ${TCL_LIBARY})
endif()
