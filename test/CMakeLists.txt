message(STATUS "Building tests")
foreach(execid
    mod_heating
    mod_cattaneo
    mod_boundary
    mod_hmatrix
    mod_material
    mod_output
    mod_setup
    mod_evolve
    mod_inputs
  )
  add_executable(test_${execid} test_${execid}.f90)
  # # Specify the include directories
  target_include_directories(test_${execid} PRIVATE "${CMAKE_BUILD_DIR}" "${CMAKE_BUILD_DIR}/mod")
  
  target_link_libraries(test_${execid} PRIVATE ${PROJECT_NAME}_library)
  add_test(NAME test_${execid} COMMAND test_${execid})
endforeach()