public class Client {
    // Private variables to store the client's information.
    private int id; // Unique identifier for the client.
    private String name; // Name of the client.
    private String district; // District where the client resides.
    private int yearsOfLoyalty; // Number of years the client has been loyal to the service.

    // Constructor to initialize a new Client object with specified details.
    public Client(int id, String name, String district, int yearsOfLoyalty) {
        this.id = id; // Sets the client's ID.
        this.name = name; // Sets the client's name.
        this.district = district; // Sets the district of the client.
        this.yearsOfLoyalty = yearsOfLoyalty; // Sets how many years the client has been loyal.
    }

    // Getters for the client's properties.
    public int getId() {
        // Returns the client's ID.
        return id;
    }

    public String getName() {
        // Returns the client's name.
        return name;
    }

    public String getDistrict() {
        // Returns the district of the client.
        return district;
    }

    public int getYearsOfLoyalty() {
        // Returns the number of years the client has been loyal.
        return yearsOfLoyalty;
    }

    // Setters for the client's properties.
    public void setId(int id) {
        // Updates the client's ID.
        this.id = id;
    }

    public void setName(String name) {
        // Updates the client's name.
        this.name = name;
    }

    public void setDistrict(String district) {
        // Updates the client's district.
        this.district = district;
    }

    public void setYearsOfLoyalty(int yearsOfLoyalty) {
        // Updates the number of years the client has been loyal.
        this.yearsOfLoyalty = yearsOfLoyalty;
    }

    @Override
    // Overrides the toString() method to provide a string representation of the Client object.
    public String toString() {
        // Formats the client's information as a string.
        return String.format("Id: %d , Nome: %s , Distrito: %s , Anos de lealdade: %d,", id, name, district, yearsOfLoyalty);
    }
}
