#!/bin/bash

find_parent_dir() {
  current_dir=$(pwd)
  while [ "$current_dir" != "/" ]; do
    if [ -d "$current_dir/heatflow-mk2" ]; then
      echo "$current_dir/heatflow-mk2"
      return
    fi
    current_dir=$(dirname "$current_dir")
  done
  echo "Parent directory 'heatflow-mk2' not found." >&2
  exit 1
}

# Get the parent dir
parent_dir=$(find_parent_dir)

# get path to executable
executable_path="$parent_dir/bin/ThermalFlow.x"
executable_dir=$(dirname "$executable_path")

# Confirm path
read -e -p "The path to ThermalFlow.x is: " -i "$executable_path" executable_path

while [[ ! -f "$executable_path" ]]; do
  echo "The file does not exist"
  exit
done

# ask for threads number
read -e -p "Enter the number of threads to use: " -i "4" Thread_num

# Create wrapper
cat << EOF > Thermal-omp.x
#!/bin/bash

print_usage() {
  echo "Usage: \$0 [-t num_threads] [-p proc_bind_option] [-d dynamic] [-h]"
  echo ""
  echo "Options:"
  echo "  -t num_threads       Set the number of OpenMP threads"
  echo "  -d dynamic           Enable or disable dynamic adjustment of threads (true or false)"
  echo "  -p proc_bind_option  Set the OpenMP PROC_BIND option (true, false, spread, close, master)"
  echo "  -h                   Show this help message and exit"
}

# Default OpenMP parameters
OMP_NUM_THREADS=$Thread_num
OMP_DYNAMIC=false
OMP_PROC_BIND=spread

while getopts "t:p:h" opt; do
  case \${opt} in
    t) OMP_NUM_THREADS=\${OPTARG} ;;
    p) OMP_PROC_BIND=\${OPTARG} ;;
    d) OMP_DYNAMIC=\${OPTARG} ;;
    h) print_usage; exit 0 ;;
    \?) echo "Invalid option: -\${OPTARG}" >&2; print_usage; exit 1 ;;
    :) echo "Option -\${OPTARG} requires an argument." >&2; print_usage; exit 1 ;;
  esac
done

export OMP_NUM_THREADS
export OMP_DYNAMIC
export OMP_PROC_BIND

# Execute the program with the specified OpenMP parameters
time $executable_path
EOF

# Make the new script executable
chmod +x Thermal-omp.x

# Move the Thermal-omp.x script to the same directory as ThermalFlow.x
mv Thermal-omp.x "$executable_dir"

echo "The Thermal-omp.x script has been created and moved to $executable_dir"
