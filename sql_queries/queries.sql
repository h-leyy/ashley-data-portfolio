/* Aider l'entreprise classicmodels à améliorer sa stratégie commerciale
l'analyse des données se fera en 5 parties :
- intro pour voir l'état de l'entreprise
- performance organisationnelle
- chiffre d'affaire de l'entreprise
- clientèle ciblée
- logistique du stock */


/* INTRO
employés et localisation
clients et localisation
produits et catégorie
intervalle des données
*/
USE CLASSICMODELS;

-- Nombre d'employés par ville
SELECT 
    classicmodels.offices.city AS city,
    COUNT(employeeNumber) AS total_employees
FROM 
    classicmodels.offices
LEFT JOIN 
    classicmodels.employees
ON 
     classicmodels.offices.officeCode =  classicmodels.employees.officeCode
GROUP BY 
     classicmodels.offices.city
ORDER BY 
    total_employees DESC;

/* il y a 23 employés*/
/* 5 villes différentes*/


-- Nombre de clients par pays
SELECT 
    country,
    COUNT(customerNumber) AS customer_per_country
FROM 
     classicmodels.customers
GROUP BY 
    country
ORDER BY 
    customer_per_country DESC
;

/* il y a 122 clients et localisation */

-- Nombre de produits par catégorie
SELECT 
    productLine AS productLine,
    COUNT(productCode) AS number_of_products_per_productLine
FROM 
     classicmodels.products
GROUP BY 
    productLine
ORDER BY 
    number_of_products_per_productLine DESC;
/* 7 catégories et le nb de produits qu'ils contiennent*/

-- Nombre total de produits
SELECT  
    COUNT(productCode) AS total_produits  
FROM  
    classicmodels.products;
/* nb total de produits*/
    
-- Analyse des commandes : nombre total, ventes et intervalle temporel
SELECT 
    COUNT(DISTINCT (classicmodels.orderdetails.orderNumber)) AS total_orders, 
    SUM( classicmodels.orderdetails.quantityOrdered) AS total_sales,        
    MIN(YEAR( classicmodels.orders.orderDate)) AS start_year,              
    MAX(YEAR( classicmodels.orders.orderDate)) AS end_year                
FROM  classicmodels.orderdetails
INNER JOIN  classicmodels.orders 
ON  classicmodels.orderdetails.orderNumber =  classicmodels.orders.orderNumber;

/* nombre de commande, total sales et intervalle de temps*/




/* performance organisationnelle
- organisation RH
- vente par bureaux en fonction de la présence des managers
*/

-- Répartition des postes et localisation des employés
SELECT 
    classicmodels.offices.city, 
    classicmodels.employees.jobTitle, 
    COUNT(classicmodels.employees.employeeNumber) 
FROM 
    classicmodels.employees 
INNER JOIN 
    classicmodels.offices 
    ON classicmodels.employees.officeCode = classicmodels.offices.officeCode 
GROUP BY 
    classicmodels.offices.city, 
    classicmodels.employees.jobTitle 
ORDER BY 
    classicmodels.offices.city ASC, 
    COUNT(classicmodels.employees.employeeNumber) DESC;

/* différents postes et les localisations*/

-- Revenu des bureaux en fonction de la présence d’un manager
SELECT 
    offices.city AS office_location, 
    COUNT(DISTINCT orders.orderNumber) AS total_orders, 
    SUM(orderdetails.quantityOrdered * orderdetails.priceEach) AS total_revenue, 
    CASE 
        WHEN COUNT(DISTINCT managers.employeeNumber) > 0 THEN 'Yes' 
        ELSE 'No' 
    END AS has_manager
FROM 
    classicmodels.offices
LEFT JOIN 
    classicmodels.employees AS managers 
    ON offices.officeCode = managers.officeCode 
    AND managers.jobTitle LIKE '%Manager%'
LEFT JOIN 
    classicmodels.employees AS sales 
    ON offices.officeCode = sales.officeCode 
LEFT JOIN 
    classicmodels.customers 
    ON sales.employeeNumber = customers.salesRepEmployeeNumber
LEFT JOIN 
    classicmodels.orders 
    ON customers.customerNumber = orders.customerNumber
LEFT JOIN 
    classicmodels.orderdetails 
    ON orders.orderNumber = orderdetails.orderNumber
GROUP BY 
    offices.city
ORDER BY 
    total_revenue DESC;

/* revenu des offices en fonction présence ou non des managers*/

-- Répartition des managers et de leurs subordonnés

    SELECT 
    managers.jobTitle AS manager_position, 
    offices.city AS manager_city, 
    subordinates.employeeNumber AS subordinate_id,
    subordinates.jobTitle AS subordinate_position,
    offices2.city AS subordinate_location
FROM 
    classicmodels.employees AS managers
INNER JOIN 
    classicmodels.offices ON managers.officeCode = offices.officeCode
LEFT JOIN 
    classicmodels.employees AS subordinates ON managers.employeeNumber = subordinates.reportsTo
LEFT JOIN 
    classicmodels.offices AS offices2 ON subordinates.officeCode = offices2.officeCode
WHERE 
    managers.jobTitle LIKE '%Manager%' -- Filtrer pour inclure uniquement les managers
ORDER BY 
    manager_city, manager_position, subordinate_location;



/* répartition des managers et leur surbordonnés*/

-- meilleur et pire vente par bureau

SELECT 
    o.city AS office_city, 
    SUM(od.quantityOrdered * od.priceEach) AS total_sales
FROM 
    classicmodels.orderdetails od
JOIN 
    classicmodels.orders ord ON od.orderNumber = ord.orderNumber
JOIN 
    classicmodels.customers c ON ord.customerNumber = c.customerNumber
JOIN 
    classicmodels.employees e ON c.salesRepEmployeeNumber = e.employeeNumber
JOIN 
    classicmodels.offices o ON e.officeCode = o.officeCode
GROUP BY 
    o.city
ORDER BY 
    total_sales DESC;


/* chiffre d'affaire
- ca total et par an 
- catégories les plus vendues
- marge moyenne
- plus faible marge
- évolution des ventes de chaque catégorie entre les 3 années
- saisonnalité des commandes
 */

-- Chiffre d’affaires total
SELECT 
    SUM(quantityOrdered * priceEach) AS total_revenue
FROM 
    classicmodels.orderdetails;
/*ca total*/


-- taux de croissance de 2003  à 2004

SELECT 
    year, 
    total_sales,
    LAG(total_sales) OVER (ORDER BY year) AS previous_year_sales,
    ROUND(((total_sales - LAG(total_sales) OVER (ORDER BY year)) / LAG(total_sales) OVER (ORDER BY year)) * 100, 2) AS growth_rate
FROM (
    SELECT 
        YEAR(ord.orderDate) AS year, 
        SUM(od.quantityOrdered * od.priceEach) AS total_sales
    FROM 
        classicmodels.orders ord
    JOIN 
        classicmodels.orderdetails od ON ord.orderNumber = od.orderNumber
    WHERE 
        YEAR(ord.orderDate) IN (2003, 2004)
    GROUP BY 
        YEAR(ord.orderDate) 
) AS yearly_sales;


-- Chiffre d’affaires annuel
SELECT 
    YEAR(classicmodels.orders.orderDate) AS year, 
    COUNT(DISTINCT MONTH(classicmodels.orders.orderDate)) AS months_in_year, 
    SUM(classicmodels.orderdetails.quantityOrdered * classicmodels.orderdetails.priceEach) AS total_revenue 
FROM 
    classicmodels.orders 
JOIN 
    classicmodels.orderdetails 
    ON classicmodels.orders.orderNumber = classicmodels.orderdetails.orderNumber 
GROUP BY 
    YEAR(classicmodels.orders.orderDate) 
ORDER BY 
    year ASC;

/* CA par année*/


-- Évolution du chiffre d’affaires par mois
SELECT 
    YEAR(classicmodels.orders.orderDate) AS year, 
    MONTH(classicmodels.orders.orderDate) AS month, 
    SUM(classicmodels.orderdetails.quantityOrdered * classicmodels.orderdetails.priceEach) AS total_revenue
FROM 
    classicmodels.orders 
JOIN 
    classicmodels.orderdetails 
    ON classicmodels.orders.orderNumber = classicmodels.orderdetails.orderNumber 
GROUP BY 
    YEAR(classicmodels.orders.orderDate), 
    MONTH(classicmodels.orders.orderDate)
ORDER BY 
    year ASC, 
    month ASC;

/* ca evolution par mois */


-- Catégories de produits les plus vendues
SELECT 
    classicmodels.products.productLine AS product_category, 
    SUM(classicmodels.orderdetails.quantityOrdered) AS total_quantity_sold 
FROM 
    classicmodels.orderdetails
JOIN 
    classicmodels.products 
    ON classicmodels.orderdetails.productCode = classicmodels.products.productCode  
GROUP BY 
    classicmodels.products.productLine 
ORDER BY 
    total_quantity_sold DESC ;

/* catégorie les plus vendues*/

-- Marge moyenne des produits
SELECT 
    ROUND(AVG((classicmodels.products.MSRP - classicmodels.products.buyPrice) / classicmodels.products.buyPrice * 100), 2) 
    AS average_margin_percentage 
FROM 
    classicmodels.products; 

/* marge moyenne*/

-- Évolution des revenus par catégorie de produits




SELECT 
    YEAR(classicmodels.orders.orderDate) AS year,
    classicmodels.products.productLine AS category,
    SUM(classicmodels.orderdetails.quantityOrdered * classicmodels.orderdetails.priceEach) AS total_revenue
FROM 
    classicmodels.orders
JOIN 
    classicmodels.orderdetails ON classicmodels.orders.orderNumber = classicmodels.orderdetails.orderNumber
JOIN 
    classicmodels.products ON classicmodels.orderdetails.productCode = classicmodels.products.productCode
GROUP BY 
    YEAR(classicmodels.orders.orderDate), classicmodels.products.productLine
ORDER BY 
    year ASC, total_revenue DESC;

/* evol des revenu des catégorie par an*/

-- Saisonnalité des commandes
SELECT 
    YEAR(classicmodels.orders.orderDate) AS year,
    month(classicmodels.orders.orderDate) AS month,
    QUARTER(classicmodels.orders.orderDate) AS quarter,
    COUNT(classicmodels.orders.orderNumber) AS total_orders,
    SUM(classicmodels.orderdetails.quantityOrdered * classicmodels.orderdetails.priceEach) AS total_sales
FROM 
    classicmodels.orders
JOIN 
    classicmodels.orderdetails ON classicmodels.orders.orderNumber = classicmodels.orderdetails.orderNumber
    
GROUP BY 
    YEAR(classicmodels.orders.orderDate), 
    MONTH(classicmodels.orders.orderDate), 
    QUARTER(classicmodels.orders.orderDate)
ORDER BY 
    YEAR(classicmodels.orders.orderDate) DESC, 
    MONTH(classicmodels.orders.orderDate) ASC;

/* saisonnalité des commandes */


-- part du CA de chaque catégorie

SELECT 
    products.productLine AS category, 
    SUM(orderdetails.quantityOrdered * orderdetails.priceEach) AS total_revenue,
    ROUND((SUM(orderdetails.quantityOrdered * orderdetails.priceEach) / 
          (SELECT SUM(quantityOrdered * priceEach) FROM orderdetails)) * 100, 2) AS revenue_share_percentage
FROM 
    orderdetails orderdetails
JOIN 
   products products ON orderdetails.productCode = products.productCode
GROUP BY 
    products.productLine
ORDER BY 
    total_revenue DESC;


/* clientèle ciblée
Profil des clients à fort chiffre d'affaires : pays, limit crédit, type de client
quel pays commande de plus et quel catégorie 
nb de client par localisation et quantité de commande
ca généré par localisation des clients*/
use classicmodels;
-- Classement des clients les plus dépensiers
SELECT customers.customerNumber, 
       customers.customerName, 
       customers.country, 
       SUM(orderdetails.quantityOrdered * orderdetails.priceEach) AS total_amount_spent, 
       COUNT(orders.orderNumber) AS total_orders 
FROM customers 
JOIN orders ON customers.customerNumber = orders.customerNumber 
JOIN orderdetails ON orders.orderNumber = orderdetails.orderNumber 
GROUP BY customers.customerNumber, customers.customerName, customers.country 
ORDER BY total_amount_spent DESC;

/* classement des entreprises qui commandent le + */

-- Nombre moyen de commandes par client
SELECT 
    AVG(order_count) AS average_orders_per_customer
FROM ( SELECT 
        classicmodels.orders.customerNumber, 
        COUNT(classicmodels.orders.orderNumber) AS order_count
    FROM 
        classicmodels.orders
    GROUP BY 
        classicmodels.orders.customerNumber
) AS client_orders;

/* moyenne commande */

-- Pays générant le plus de chiffre d’affaires
SELECT 
    classicmodels.customers.country,
    SUM(classicmodels.payments.amount) AS total_revenue
FROM 
    classicmodels.customers
JOIN 
    classicmodels.payments ON classicmodels.customers.customerNumber = classicmodels.payments.customerNumber
GROUP BY 
    classicmodels.customers.country
ORDER BY 
    total_revenue DESC
LIMIT 5;

/* pays qui génère + de ca */


/* logistique
- turn over
- comparer stock au nb de commande
- traitement en moyenne des commandes
- les commentaires sur l'avancement des commandes
*/

   

-- Temps moyen de traitement des commandes
SELECT 
    ROUND(AVG(DATEDIFF(classicmodels.orders.shippedDate, classicmodels.orders.orderDate)), 2) AS avg_processing_time_days
FROM 
    classicmodels.orders
WHERE 
    classicmodels.orders.shippedDate IS NOT NULL;

/* moyenne générale de traitement de commande*/


-- Répartition des statuts des commandes
SELECT 
    classicmodels.orders.status,
    COUNT(classicmodels.orders.orderNumber) AS total_orders,
    ROUND(COUNT(classicmodels.orders.orderNumber) * 100.0 / (SELECT COUNT(*) FROM classicmodels.orders), 2) AS percentage
FROM 
    classicmodels.orders
GROUP BY 
    classicmodels.orders.status
ORDER BY 
    total_orders DESC;

/* evolution des commandes*/

-- Commentaires pour certaines commandes
SELECT DISTINCT 
    classicmodels.orders.comments  
FROM 
    classicmodels.orders
WHERE 
    classicmodels.orders.comments IS NOT NULL
ORDER BY 
    classicmodels.orders.comments;
/* les différents commentaires associés à certaines commandes */

-- Délai de traitement par office
SELECT 
    classicmodels.offices.officeCode,
    classicmodels.offices.city,
    COUNT(classicmodels.orders.orderNumber) AS total_orders,
    ROUND(AVG(DATEDIFF(classicmodels.orders.shippedDate, classicmodels.orders.orderDate)), 2) AS avg_processing_time,
    ROUND(AVG(DATEDIFF(classicmodels.orders.shippedDate, classicmodels.orders.orderDate)), 2) AS global_avg_processing_time
FROM 
    classicmodels.orders
JOIN 
    classicmodels.customers ON classicmodels.orders.customerNumber = classicmodels.customers.customerNumber
JOIN 
    classicmodels.employees ON classicmodels.customers.salesRepEmployeeNumber = classicmodels.employees.employeeNumber
JOIN 
    classicmodels.offices ON classicmodels.employees.officeCode = classicmodels.offices.officeCode
WHERE 
    classicmodels.orders.shippedDate IS NOT NULL
GROUP BY 
    classicmodels.offices.officeCode, classicmodels.offices.city
ORDER BY 
    avg_processing_time DESC;

/* délai de traitement par office*/

-- produit jamais vendu

SELECT products. *
FROM classicmodels.products 
LEFT JOIN classicmodels.orderdetails 
ON products.productCode = orderdetails.productCode
WHERE orderdetails.productCode IS NULL;





SELECT 
    products.productCode, 
    products.productName, 
    products.quantityInStock, 
    SUM(orderdetails.quantityOrdered) AS total_sold, 
    ROUND(SUM(orderdetails.quantityOrdered) / NULLIF(products.quantityInStock + SUM(orderdetails.quantityOrdered), 0), 2) AS stock_turnover
FROM 
    products
LEFT JOIN 
    orderdetails ON products.productCode = orderdetails.productCode
GROUP BY 
    products.productCode, products.productName, products.quantityInStock
ORDER BY 
    stock_turnover DESC;

-- comparaison des stocks
SELECT 
    products.productCode, 
    products.productName, 
    products.quantityInStock, 
    COALESCE(SUM(orderdetails.quantityOrdered), 0) AS total_sold, 
    ROUND(COALESCE(SUM(orderdetails.quantityOrdered), 0) / NULLIF(products.quantityInStock, 0), 2) AS stock_turnover,
    CASE 
        WHEN COALESCE(SUM(orderdetails.quantityOrdered), 0) = 0 THEN 'No Sales'
        WHEN products.quantityInStock = 0 THEN 'Out of Stock'
        WHEN COALESCE(SUM(orderdetails.quantityOrdered), 0) > products.quantityInStock THEN 'Low Stock'
        WHEN COALESCE(SUM(orderdetails.quantityOrdered), 0) < products.quantityInStock / 2 THEN 'Overstocked'
        ELSE 'Optimal Stock'
    END AS stock_status
FROM 
    products
LEFT JOIN 
    orderdetails ON products.productCode = orderdetails.productCode
GROUP BY 
    products.productCode, products.productName, products.quantityInStock
ORDER BY 
    stock_status DESC, stock_turnover ASC;
