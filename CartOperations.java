import java.io.*;
import java.util.*;
import java.util.logging.*;

public class CartOperations {
    // Logger to log information, warnings, and errors.
    private static final Logger LOGGER = Logger.getLogger(CartOperations.class.getName());
    // Constant for maximum number of retry attempts for the process execution.
    private static final int MAX_ATTEMPTS = 3;

    // Path to the OCaml executable to run calculations or operations.
    private String ocamlExecutablePath;
    // The district parameter that may affect the cart processing logic.
    private String district;
    // Customer loyalty in years, could influence processing logic.
    private int yearsOfLoyalty;
    // Serialized string representation of a shopping cart.
    private String cartStr;
    // List to store lines of output from the executed OCaml process.
    private List<String> resultLines;

    // Constructor initializing fields with parameters to setup operations.
    public CartOperations(String ocamlExecutablePath, String district, int yearsOfLoyalty, String cartStr) {
        this.ocamlExecutablePath = ocamlExecutablePath;
        this.district = district;
        this.yearsOfLoyalty = yearsOfLoyalty;
        this.cartStr = cartStr;
        this.resultLines = new ArrayList<>();
    }

    // Main method to execute the cart operations by running the external OCaml program.
    public void executeCartOperations() {
        int attempts = 0;
        boolean isSuccessful = false;

        while (attempts < MAX_ATTEMPTS && !isSuccessful) {
            attempts++;
            Process process = createProcess();

            if (process != null) {
                isSuccessful = handleProcessOutput(process);
                if (!isSuccessful) {
                    readErrorStream(process);
                }
            }
        }

        // Logs a warning if the process fails after the maximum number of attempts.
        if (!isSuccessful) {
            LOGGER.warning("Failed to execute OCaml program after " + MAX_ATTEMPTS + " attempts.");
        }
    }

    // Method to create a process using ProcessBuilder with the provided parameters.
    private Process createProcess() {
        try {
            ProcessBuilder processBuilder = new ProcessBuilder(
                ocamlExecutablePath,
                cartStr,
                String.valueOf(yearsOfLoyalty),
                district
            );
            return processBuilder.start();
        } catch (IOException e) {
            LOGGER.log(Level.SEVERE, "Error starting the OCaml executable", e);
            return null;
        }
    }

    // Handles the process output, checks for successful execution and reads output if successful.
    private boolean handleProcessOutput(Process process) {
        try {
            int exitCode = process.waitFor();
            if (exitCode == 0) {
                readProcessOutput(process);
                return true;
            } else {
                LOGGER.warning("OCaml program terminated with error code: " + exitCode);
                return false;
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            LOGGER.log(Level.SEVERE, "Process was interrupted", e);
            return false;
        }
    }

    // Reads the standard output of the process and stores it in resultLines.
    private void readProcessOutput(Process process) {
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()))) {
            String line;
            while ((line = reader.readLine()) != null) {
                resultLines.add(line);
            }
        } catch (IOException e) {
            LOGGER.log(Level.SEVERE, "Failed to read process output", e);
        }
    }

    // Reads the error stream of the process and logs the errors.
    private void readErrorStream(Process process) {
        try (BufferedReader errorReader = new BufferedReader(new InputStreamReader(process.getErrorStream()))) {
            String line;
            while ((line = errorReader.readLine()) != null) {
                LOGGER.severe(line);
            }
        } catch (IOException e) {
            LOGGER.log(Level.SEVERE, "Failed to read error stream", e);
        }
    }

    // Returns a copy of the resultLines to ensure immutability of the internal state.
    public List<String> getResultLines() {
        return new ArrayList<>(resultLines);
    }
}
