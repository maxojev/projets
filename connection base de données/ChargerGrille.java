package chabi.wacquet.modele;

import java.sql.*;

public class ChargerGrille
{
    private Connection maConnexion ;
    public ChargerGrille() {
        try { maConnexion = connectionMySQL() ; }
        catch(SQLException e) { e.printStackTrace(); }
    }


    public static Connection connectionMySQL() throws SQLException {
        String url = "jdbc:mysql://anteros.istic.univ-rennes1.fr/base_bousse";
        try { Class.forName("com.mysql.jdbc.Driver"); }
        catch (ClassNotFoundException e) { e.printStackTrace(); }
        return DriverManager.getConnection(url, "user", "pw"); // Ayant travaillé sur une base locale, les identifiants sont à saisir
    }


    public static MotsCroisesTP5 extraireBD(Connection connect, int grille) throws SQLException { // Récupération d'une grille aléatoire de la bdd
        // Récupération de la taille pour l'initialisation de la grille
        PreparedStatement pstmt = connect.prepareStatement("SELECT largeur, hauteur FROM tp5_grille WHERE num_grille = ?");
        pstmt.setInt(1, grille);
        ResultSet resGrille = pstmt.executeQuery();
        resGrille.next();
        MotsCroisesTP5 mc = new MotsCroisesTP5(resGrille.getInt("hauteur"), resGrille.getInt("largeur"));
        pstmt.close();
        pstmt = null;
        // On enregistre dans l'instance de MotsCroises les infos (solution, définition...)
        PreparedStatement pstmt2 = connect.prepareStatement("SELECT * FROM tp5_mot WHERE num_grille = ?");
        pstmt2.setInt(1, grille);
        ResultSet tabMots = pstmt2.executeQuery();
        while (tabMots.next())
        {
            int ligne = tabMots.getInt("ligne");
            int colonne = tabMots.getInt("colonne");
            boolean horiz = tabMots.getInt("horizontal") == 1;
            String solution = tabMots.getString("solution");
            mc.setDefinition(ligne, colonne, horiz, tabMots.getString("definition"));
            for (int i = 0; i < solution.length(); i++)
            {
                if (horiz) {
                    mc.setSolution(ligne, colonne + i, solution.charAt(i));
                } else {
                    mc.setSolution(ligne + i, colonne, solution.charAt(i));
                }
            }
        }
        pstmt2.close();
        pstmt2 = null;
        connect.close();
        connect = null;
        return mc;
    }

    public MotsCroisesTP5 extraireGrille(int random) throws SQLException {
        return extraireBD(maConnexion, random);
    }


}
