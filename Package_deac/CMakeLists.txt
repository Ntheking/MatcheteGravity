cmake_minimum_required(VERSION 3.29)
project(Package C)

set(CMAKE_C_STANDARD 11)

add_executable(Package
    GroupMagic/Kernel/init.m
    GroupMagic/CGManipulations.m
    GroupMagic/GroupMagic.m
    GroupMagic/Groups.m
    GroupMagic/OtherDefinitions.m
    Checks.m
    CovariantDerivative.m
    Definitions.m
    DiracAlgebra.m
    FieldRedef.m
    FunctionalTools.m
    Indices.m
    LoopIntegration.m
    Matchete.m
    Matching.m
    NCM.m
    Printing.m
    Simplifications.m
    SuperTrace.m
    TreeLevelMatching.m)
