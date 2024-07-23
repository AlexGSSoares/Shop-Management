import org.jpl7.*;
import java.util.*;
import java.io.*;

public class ManageClient {
    // Constant for the path to the Prolog file used for queries.
    private static final String prologFile = "store.pl";
    // Scanner object for reading input from the console.
    private Scanner scanner;

    // Constructor initializes the scanner used for input.
    public ManageClient(Scanner scanner) {
        this.scanner = scanner;
    }

    // Main method that handles the client management interface.
    public void runManageclient() {
        // Initial query to consult the Prolog file for data manipulation.
        Query q1 = new Query("consult", new Term[]{new Atom(prologFile)});
        System.out.println("Consult " + (q1.hasSolution() ? "succeeded" : "FAILED"));

        int choice;
        do {
            // Display menu options to the user.
            System.out.println("\nChoose an Option:");
            System.out.println("1 - View All Clients");
            System.out.println("2 - View Clients by District");
            System.out.println("3 - View Purchase History by Client");
            System.out.println("4 - View Clients by Loyalty");
            System.out.println("5 - Manage Client");
            System.out.println("0 - Exit");
            System.out.print("Enter your choice: ");
            choice = scanner.nextInt();
            scanner.nextLine(); // Clear the buffer after reading an integer.

            // Handle user's choice.
            switch (choice) {
                case 1:
                    displayAllClients();
                    break;
                case 2:
                    displayClientsByDistrict();
                    break;
                case 3:
                    displayPurchaseHistoryByClient();
                    break;
                case 4:
                    displayClientsByLoyalty();
                    break;
                case 5:
                    manageClients();
                    break;
                case 0:
                    System.out.println("Exiting...");
                    break;
                default:
                    System.out.println("Invalid choice, please choose again.");
            }
        } while (choice != 0);
        scanner.close();
    }

    // Displays all clients using a Prolog query.
    private static void displayAllClients() {
        Query q2 = new Query("all_clients(Clients)");
        processClientsQuery(q2);
    }

    // Displays clients filtered by district.
    private void displayClientsByDistrict() {
        System.out.print("Enter district to filter by: ");
        String district = scanner.nextLine().trim();
        Query q2 = new Query("clients_by_district", new Term[] {new Atom(district), new Variable("Clients")});
        processClientsQuery(q2);
    }

    // Displays the purchase history for a specific client.
    private void displayPurchaseHistoryByClient() {
        System.out.print("Enter client ID: ");
        int clientID = scanner.nextInt(); scanner.nextLine();
        Query q2 = new Query("client_purchases", new Term[] {new org.jpl7.Integer(clientID), new Variable("Purchases")});
        processPurchasesQuery(q2);
    }

    // Displays clients filtered by the number of loyalty years.
    private void displayClientsByLoyalty() {
        System.out.print("Enter minimum loyalty years: ");
        int minLoyalty = scanner.nextInt(); scanner.nextLine();
        Query q2 = new Query("clients_by_loyalty", new Term[] {new org.jpl7.Integer(minLoyalty), new Variable("Clients")});
        processClientsQuery(q2);
    }

    // Sub-menu to manage individual client entries.
    private void manageClients() {
        System.out.println("\nManage Clients:");
        System.out.println("1 - Add a new client");
        System.out.println("2 - Modify an existing client");
        System.out.println("3 - Remove a client");
        System.out.println("0 - Return to main menu");
        int clientChoice = scanner.nextInt();
        scanner.nextLine();

        switch (clientChoice) {
            case 1:
                addClient();
                break;
            case 2:
                modifyClient();
                break;
            case 3:
                removeClient();
                break;
            case 0:
                break;
            default:
                System.out.println("Invalid choice, please choose again.");
        }
    }

    // Adds a new client after ensuring the ID is unique.
    private void addClient() {
        System.out.print("Enter client ID: ");
        int id = scanner.nextInt(); scanner.nextLine();
        if (clientExists(id)) {
            System.out.println("A client with this ID already exists. Please use a different ID.");
            return;
        }
    
        System.out.print("Enter client name: ");
        String name = scanner.nextLine().trim();
        System.out.print("Enter client district: ");
        String district = scanner.nextLine().trim();
        System.out.print("Enter client loyalty years: ");
        int loyaltyYears = scanner.nextInt(); scanner.nextLine();
    
        if (updateClient("add_client", id, name, district, loyaltyYears)) {
            System.out.println("Client added successfully.");
        } else {
            System.out.println("Failed to add client.");
        }
    }
    
    // Modifies an existing client's details.
    private void modifyClient() {
        System.out.print("Enter client ID: ");
        int id = scanner.nextInt(); scanner.nextLine();
        System.out.print("Enter new client name: ");
        String newName = scanner.nextLine().trim();
        System.out.print("Enter new client district: ");
        String newDistrict = scanner.nextLine().trim();
        System.out.print("Enter new loyalty years: ");
        int newLoyaltyYears = scanner.nextInt(); scanner.nextLine();

        if (updateClient("modify_client", id, newName, newDistrict, newLoyaltyYears)) {
            System.out.println("Client modified successfully.");
        } else {
            System.out.println("Failed to modify client.");
        }
    }

    // Removes a client from the system.
    private void removeClient() {
        System.out.print("Enter client ID to remove: ");
        int id = scanner.nextInt(); scanner.nextLine();

        if (updateClient("remove_client", id, "", "", 0)) {
            System.out.println("Client removed successfully.");
        } else {
            System.out.println("Failed to remove client.");
        }
    }

    // Checks if a client exists using a Prolog query.
    private static boolean clientExists(int id) {
        Query query = new Query("client", new Term[]{new org.jpl7.Integer(id), new Variable("Name"), new Variable("District"), new Variable("LoyaltyYears")});
        return query.hasSolution();
    }

    // Updates client data in the Prolog file.
    private static boolean updateClient(String action, int id, String name, String district, int loyaltyYears) {
        List<String> fileContent = new ArrayList<>();
        boolean updateMade = false;
        int lastClientIndex = -1;  // Keeps track of the last client index found.
    
        try (BufferedReader br = new BufferedReader(new FileReader(prologFile))) {
            String line;
            int lineIndex = 0;
            while ((line = br.readLine()) != null) {
                if (line.startsWith("client(")) {
                    lastClientIndex = lineIndex;  // Updates whenever a client is found.
                    if (line.startsWith("client(" + id + ",") && action.equals("modify_client")) {
                        line = String.format("client(%d, '%s', '%s', %d).", id, name, district, loyaltyYears);
                        updateMade = true;
                    } else if (line.startsWith("client(" + id + ",") && action.equals("remove_client")) {
                        // Skip the current line, not adding it to fileContent.
                        updateMade = true;
                        lineIndex++;
                        continue;
                    }
                }
                fileContent.add(line);
                lineIndex++;
            }
    
            if (action.equals("add_client") && !updateMade) {
                String newClient = String.format("client(%d, '%s', '%s', %d).", id, name, district, loyaltyYears);
                if (lastClientIndex >= 0) {
                    fileContent.add(lastClientIndex + 1, newClient);  // Adds new client right after the last client found.
                } else {
                    fileContent.add(newClient);  // Adds to the end if there are no other clients.
                }
                updateMade = true;
            }
        } catch (IOException e) {
            e.printStackTrace();
            return false;
        }
    
        // Rewrites the file with the new content.
        try (BufferedWriter bw = new BufferedWriter(new FileWriter(prologFile))) {
            for (String line : fileContent) {
                bw.write(line);
                bw.newLine();
            }
        } catch (IOException e) {
            e.printStackTrace();
            return false;
        }
    
        return updateMade;
    }

    // Processes client queries and prints results.
    private static void processClientsQuery(Query query) {
        ArrayList<Client> clients = new ArrayList<>();
        Map<String, Term>[] solutions = query.allSolutions();
        if (solutions != null) {
            for (Map<String, Term> solution : solutions) {
                Term clientList = solution.get("Clients");
                for (Term client : clientList.toTermArray()) {
                    Term[] details = client.toTermArray();
                    if (!(details[0] instanceof org.jpl7.Integer &&
                          details[1] instanceof org.jpl7.Atom &&
                          details[2] instanceof org.jpl7.Atom &&
                          details[3] instanceof org.jpl7.Integer)) {
                        System.out.println("Unexpected data types in Prolog response.");
                        continue;
                    }
                    Client newClient = new Client(
                        details[0].intValue(),
                        details[1].name(),
                        details[2].name(),
                        details[3].intValue()
                    );
                    clients.add(newClient);
                }
            }
        }

        if (clients.isEmpty()) {
            System.out.println("No clients found.");
        } else {
            for (Client client : clients) {
                System.out.println(client);
            }
        }
    }

    // Processes purchase queries and prints results.
    private static void processPurchasesQuery(Query query) {
        ArrayList<Purchase> purchases = new ArrayList<>();
        Map<String, Term>[] solutions = query.allSolutions();
        if (solutions != null) {
            for (Map<String, Term> solution : solutions) {
                Term purchaseList = solution.get("Purchases");
                for (Term purchase : purchaseList.toTermArray()) {
                    Term[] details = purchase.toTermArray();
                    try {
                        String date = details[0].name();
                        double preDiscountAmount = details[1].doubleValue();
                        double categoryDiscount = details[2].doubleValue();
                        double loyaltyDiscount = details[3].doubleValue();
                        double shippingCost = details[4].doubleValue();
                        double total = details[5].doubleValue();
                        Purchase newPurchase = new Purchase(date, preDiscountAmount, categoryDiscount, loyaltyDiscount, shippingCost, total);
                        purchases.add(newPurchase);
                    } catch (Exception e) {
                        System.out.println("Error processing purchase data: " + e.getMessage());
                    }
                }
            }
        }

        if (purchases.isEmpty()) {
            System.out.println("No purchase history found for the specified client.");
        } else {
            for (Purchase purchase : purchases) {
                System.out.println(purchase);
            }
        }
    }    
}
