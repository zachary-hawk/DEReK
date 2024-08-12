
import argparse
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import matplotlib as mpl
import math
import re
from matplotlib.ticker import FuncFormatter, LogLocator


def configure_plot_style():
    # Enable LaTeX for text rendering
    mpl.rc('text', usetex = True)
    plt.style.use("classic")
    
    mpl.rcParams['mathtext.fontset'] = 'stix'
    mpl.rcParams['font.family'] = 'STIXGeneral'


    #mpl.rcParams['text.usetex'] = True
    #mpl.rcParams['font.family'] = 'serif'
    #mpl.rcParams['font.serif'] = ['Computer Modern Roman']
    ##mpl.rcParams['text.latex.preamble'] = r'\usepackage{amsmath}'
    
    # Set the style for publication quality

    # Configure global matplotlib settings for a more professional look
    mpl.rcParams['axes.facecolor'] = 'white'
    mpl.rcParams['figure.facecolor'] = 'white'
    mpl.rcParams['savefig.facecolor'] = 'white'
    mpl.rcParams['grid.color'] = 'white'

    mpl.rcParams['figure.figsize'] = (10, 6)
    mpl.rcParams['axes.titlesize'] = 18
    mpl.rcParams['axes.labelsize'] = 16
    mpl.rcParams['xtick.labelsize'] = 14
    mpl.rcParams['ytick.labelsize'] = 14
    mpl.rcParams['legend.fontsize'] = 14
    mpl.rcParams['lines.linewidth'] = 2
    mpl.rcParams['lines.markersize'] = 6
    mpl.rcParams['savefig.dpi'] = 300
    mpl.rcParams['savefig.format'] = 'png'
    mpl.rcParams['savefig.bbox'] = 'tight'



def plot_memory_report(files):
    def format_memory(value, pos):
        if value == 0:
            return '0 B'
        units = ['B', 'KB', 'MB', 'GB', 'TB', 'PB', 'EB', 'ZB', 'YB']
        power = int(np.log2(value) // 10)  # Each unit represents 1024**1, 1024**2, ...
        scaled_value = value / float(1024 ** power)
        return f'{scaled_value:.0f} {units[power]}'


    num_files = len(files)
    
    # Determine grid size
    cols = int(math.ceil(math.sqrt(num_files)))
    rows = int(math.ceil(num_files / cols))

    fig, axs = plt.subplots(rows, cols, figsize=(cols * 5, rows * 4), sharey='row',sharex='col')
    axs = axs.flatten()  # Flatten the array for easy iteration
    
    handles, labels = [], []
    
    #fig = plt.figure(figsize=(cols * 5, rows * 4))
    #grid = mpl.gridspec.GridSpec(rows, cols, wspace=0.4, hspace=0.4)
    
    # Track axes for shared y-axis
    ax_y_shared = None
    
    for idx, file_name in enumerate(files):
        try:

            match = re.search(r'\d{4}', file_name)
            if match:
                core_id = str(int(match.group()))

            # Read the .mem file using numpy
            data = np.loadtxt(file_name)
            
            # Convert to pandas DataFrame and assign column names
            columns = ['Time','IO','Basis','Wavefunction','Potential','Density','General','Total']
            data = pd.DataFrame(data, columns=columns)
            
            if data.empty:
                print(f"Warning: The file '{file_name}' is empty or contains no data.")
                continue
            
            # Determine subplot position
            row_idx = idx // cols
            col_idx = idx % cols

            #the One Below:
            oneBelow=(row_idx+1)*cols+col_idx
            

            
            # Create subplot for this file
            ax = axs[idx]
            #ax.set_title(file_name)
            
            #ax = fig.add_subplot(grid[row_idx, col_idx], sharey=ax_y_shared)
            #ax.set_title(file_name)
            
            # PlotÂ each memory column against time
            for col in columns[1:]:  # Skip the 'Time (s)' column
                if row_idx==0 and col_idx==0:
                    ax.plot(data['Time'], data[col], label=col)

                else:
                    ax.plot(data['Time'], data[col])

            
            # Add a legend only for the first subplot in each row
            #if col_idx == 0:
            #    ax.legend()
            
            # Label the axes
            if row_idx==rows or oneBelow>=num_files:
                ax.set_xlabel(r'Time (s)')
            if col_idx==0:
                ax.set_ylabel(r'Memory (B)')

            ax.xaxis.set_tick_params(which='both', labelbottom=True)
            ax.yaxis.set_tick_params(which='both', labelleft=True)


            #ax.set_yscale('log', base=2)
            tick_locations = [1, 1024, 1024**2, 1024**3, 1024**4]
            ax.set_yticks(tick_locations)
            #ax.set_yticklabels(['1 B', '1 KB', '1 MB', '1 GB', '1 TB'])

            # Enable the outermost box (spines) and remove the grid
            ax.spines['top'].set_visible(True)
            ax.spines['right'].set_visible(True)
            ax.spines['bottom'].set_visible(True)
            ax.spines['left'].set_visible(True)
            ax.grid(False)  # Disable the internal grid lines


            ax.text(0.05, 0.95, f'core: {core_id}', transform=ax.transAxes,
                    verticalalignment='top', horizontalalignment='left',
                    fontsize=mpl.rcParams['axes.labelsize'], bbox=dict(facecolor='white', alpha=0.8, edgecolor='none'))
            
            
            ## Share y-axis among subplots in the same row
            #if ax_y_shared is None:
            #    ax_y_shared = ax.yaxis
            #    print(idx)
            #else:
            #    ax.yaxis.set_tick_params(which='both', labelleft=False)
            
        except OSError:
            print(f"Error: The file '{file_name}' could not be read. Please check the file path.")
        except ValueError:
            print(f"Error: The file '{file_name}' contains non-numeric data or is improperly formatted.")

    for idx in range(len(files), len(axs)):
        fig.delaxes(axs[idx])
    # Improve layout and aesthetics
    plt.figlegend(loc=9,ncol=7)
    #plt.tight_layout()
    plt.tight_layout(rect=[0, 0, 1, 0.95])
    
    # Save the figure as a high-resolution PNG file
    plt.savefig('memory_reports.png')
    
    # Display the plot
    plt.show()

def main():
    parser = argparse.ArgumentParser(description="Plot memory usage from .mem files")
    parser.add_argument('files', metavar='F', type=str, nargs='+', help='an input file with extension .mem')
    args = parser.parse_args()
    
    configure_plot_style()
    
    plot_memory_report(args.files)

if __name__ == "__main__":
    main()


