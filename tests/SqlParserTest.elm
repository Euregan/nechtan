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

createUserTable : String
createUserTable =
  """CREATE TABLE utilisateur
(
  id INT PRIMARY KEY NOT NULL,
  firstName VARCHAR(100),
  lastName VARCHAR(100),
  email VARCHAR(255),
  birthday DATE,
  country VARCHAR(255),
  city VARCHAR(255),
  zipCode VARCHAR(5),
  purchaseCount INT
);"""

createCountryTable : String
createCountryTable =
  """CREATE TABLE `Country` (
  `CountryId` char(3) COLLATE latin1_general_ci NOT NULL DEFAULT '',
  `Name` varchar(100) COLLATE latin1_general_ci NOT NULL DEFAULT '',
  `Language` char(2) COLLATE latin1_general_ci NOT NULL DEFAULT '',
  `RegionId` char(20) COLLATE latin1_general_ci,
  PRIMARY KEY (`CountryId`,`Language`)
  CONSTRAINT `region_idfk` FOREIGN KEY (`RegionId`) REFERENCES `Region` (`RegionId`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;"""

createRegionTable : String
createRegionTable =
  """CREATE TABLE `Region` (
  `RegionId` char(20) COLLATE latin1_general_ci NOT NULL,
  `Name` varchar(100) COLLATE latin1_general_ci NOT NULL,
  `Position` tinyint NOT NULL UNIQUE,
  PRIMARY KEY (`RegionId`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_general_ci;"""

userTable : Table
userTable =
  Table
    "utilisateur"
    [ Field "id" INT [PrimaryKey, NotNullable]
    , Field "firstName" (VARCHAR 100) []
    , Field "lastName" (VARCHAR 100) []
    , Field "email" (VARCHAR 255) []
    , Field "birthday" DATE []
    , Field "country" (VARCHAR 255) []
    , Field "city" (VARCHAR 255) []
    , Field "zipCode" (VARCHAR 5) []
    , Field "purchaseCount" INT []
    ]

suite : Test
suite =
  describe "The SqlParserModule"
    [ describe "create table"
      [ test "works for a complete table declaration" <|
        \_ -> Expect.equal (Ok userTable) (run createTable createUserTable)
      ]
    ]
