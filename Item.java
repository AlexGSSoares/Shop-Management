public class Item {
    // Private fields to encapsulate the item's properties.
    private int id; // Unique identifier for the item.
    private String name; // Name of the item.
    private String category; // Category to which the item belongs.
    private double price; // Price of the item.
    private int quantity; // Quantity of the item available or in the cart.

    // Constructor to initialize an Item object with specified details.
    public Item(int id, String name, String category, double price, int quantity) {
        this.id = id; // Sets the item's ID.
        this.name = name; // Sets the item's name.
        this.category = category; // Sets the item's category.
        this.price = price; // Sets the item's price.
        this.quantity = quantity; // Sets the item's quantity.
    }

    // Getters for the item's properties.
    public int getId() {  
        // Returns the item's ID.
        return id;
    }

    public String getName() {
        // Returns the item's name.
        return name;
    }

    public String getCategory() {
        // Returns the item's category.
        return category;
    }

    public double getPrice() {
        // Returns the item's price.
        return price;
    }

    public int getQuantity() {
        // Returns the item's quantity.
        return quantity;
    }

    // Setters for the item's properties.
    public void setId(int id) {
        // Updates the item's ID.
        this.id = id;
    }

    public void setName(String name) {
        // Updates the item's name.
        this.name = name;
    }

    public void setCategory(String category) {
        // Updates the item's category.
        this.category = category;
    }

    public void setPrice(double price) {
        // Updates the item's price.
        this.price = price;
    }

    public void setQuantity(int quantity) {
        // Updates the item's quantity.
        this.quantity = quantity;
    }

    // Method to format the item's information into a string suitable for a cart.
    public String formatForCart() {
        // Formats the item's details into a string using a semicolon-separated format.
        return String.format("%d;'%s';'%s';%.2f;%d", id, name, category, price, quantity);
    }

    @Override
    // Overrides the toString() method to provide a string representation of the Item object.
    public String toString() {
        // Formats the item's information as a readable string showing the ID, name, category, price, and quantity.
        return String.format("%d - %s, %s, %.2f€, %d pcs", id, name, category, price, quantity);
    }
}
