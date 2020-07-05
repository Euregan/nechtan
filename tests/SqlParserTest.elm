module SqlParserTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Parser exposing (run)
import SqlParser exposing (..)
import Data.Type exposing (Type(..))
import Data.Modifier exposing (Modifier(..))
import Data.Field exposing (Field)
import Data.Table exposing (Table)
import Data.Database exposing (Database)

createMarketplaceDatabase : String
createMarketplaceDatabase =
  "CREATE DATABASE marketplace;"

createUserTable : String
createUserTable =
  """CREATE TABLE user
(
  id INT PRIMARY KEY NOT NULL,
  firstName VARCHAR(100),
  lastName VARCHAR(100),
  email VARCHAR(255),
  address1 VARCHAR(255),
  address2 VARCHAR(255),
  address3 VARCHAR(255),
  country VARCHAR(255),
  city VARCHAR(255),
  zipCode VARCHAR(5)
);"""

createOrderTable : String
createOrderTable =
  """CREATE TABLE order
(
  id INT PRIMARY KEY NOT NULL,
  user INT,
  FOREIGN KEY (user) REFERENCES user(id)
);"""

createOrderProductTable : String
createOrderProductTable =
  """CREATE TABLE order_product
(
  order INT,
  product INT,
  FOREIGN KEY (order) REFERENCES order(id),
  FOREIGN KEY (product) REFERENCES product(id)
);"""

createProductTable : String
createProductTable =
  """CREATE TABLE product
(
  id INT PRIMARY KEY NOT NULL,
  name VARCHAR(100) NOT NULL,
  price FLOAT NOT NULL
);"""

userTable : Table
userTable =
  Table
    "user"
    [ Field "id" INT [PrimaryKey, NotNullable]
    , Field "firstName" (VARCHAR 100) []
    , Field "lastName" (VARCHAR 100) []
    , Field "email" (VARCHAR 255) []
    , Field "address1" (VARCHAR 255) []
    , Field "address2" (VARCHAR 255) []
    , Field "address3" (VARCHAR 255) []
    , Field "country" (VARCHAR 255) []
    , Field "city" (VARCHAR 255) []
    , Field "zipCode" (VARCHAR 5) []
    ]

orderTable : Table
orderTable =
  Table
    "order"
    [ Field "id" INT [PrimaryKey, NotNullable]
    , Field "user" INT [ForeignKey "user" "id"]
    ]

orderProductTable : Table
orderProductTable =
  Table
    "order_product"
    [ Field "order" INT [ForeignKey "order" "id"]
    , Field "product" INT [ForeignKey "product" "id"]
    ]

productTable : Table
productTable =
  Table
    "product"
    [ Field "id" INT [PrimaryKey, NotNullable]
    , Field "name" (VARCHAR 100) [NotNullable]
    , Field "price" FLOAT [NotNullable]
    ]

marketplaceDatabase : Database
marketplaceDatabase =
  Database
    "marketplace"
    [ userTable
    , orderTable
    , orderProductTable
    , productTable
    ]

suite : Test
suite =
  describe "The SqlParserModule"
    [ describe "create table"
      [ test "works for complete table declarations" <|
        \_ -> Expect.equal [Ok userTable, Ok orderTable, Ok orderProductTable, Ok productTable] <|
          List.map (run createTable) [createUserTable, createOrderTable, createOrderProductTable, createProductTable]
      ]
    , describe "create database"
      [ test "works for a complete database declaration" <|
        \_ -> Expect.equal (Ok marketplaceDatabase) <|
          run createDatabase (List.foldr (\curr acc -> curr ++ "\n" ++ acc) "" [createMarketplaceDatabase, createUserTable, createOrderTable, createOrderProductTable, createProductTable])
      ]
    ]
