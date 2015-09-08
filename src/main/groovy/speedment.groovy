import com.speedment.config.parameters.*

name = "socialnetwork";
packageLocation = "src/main/java";
packageName = "com.company.speedment.test";
enabled = true;
dbms {
    ipAddress = "speedment.com";
    name = "db0";
    port = 3306;
    typeName = "MySQL";
    username = "socialnetwork";
    enabled = true;
    schema {
        columnCompressionType = ColumnCompressionType.NONE;
        fieldStorageType = FieldStorageType.WRAPPER;
        name = "socialnetwork";
        schemaName = "socialnetwork";
        storageEngineType = StorageEngineType.ON_HEAP;
        defaultSchema = false;
        enabled = true;
        table {
            columnCompressionType = ColumnCompressionType.INHERIT;
            fieldStorageType = FieldStorageType.INHERIT;
            name = "image";
            storageEngineType = StorageEngineType.INHERIT;
            tableName = "image";
            enabled = true;
            column {
                columnCompressionType = ColumnCompressionType.INHERIT;
                fieldStorageType = FieldStorageType.INHERIT;
                mapping = java.lang.Long.class;
                name = "id";
                autoincrement = true;
                enabled = true;
                nullable = false;
            }
            column {
                columnCompressionType = ColumnCompressionType.INHERIT;
                fieldStorageType = FieldStorageType.INHERIT;
                mapping = java.lang.Long.class;
                name = "uploader";
                autoincrement = false;
                enabled = true;
                nullable = false;
            }
            column {
                columnCompressionType = ColumnCompressionType.INHERIT;
                fieldStorageType = FieldStorageType.INHERIT;
                mapping = java.lang.String.class;
                name = "title";
                autoincrement = false;
                enabled = true;
                nullable = false;
            }
            column {
                columnCompressionType = ColumnCompressionType.INHERIT;
                fieldStorageType = FieldStorageType.INHERIT;
                mapping = java.lang.String.class;
                name = "description";
                autoincrement = false;
                enabled = true;
                nullable = false;
            }
            column {
                columnCompressionType = ColumnCompressionType.INHERIT;
                fieldStorageType = FieldStorageType.INHERIT;
                mapping = java.lang.String.class;
                name = "img_data";
                autoincrement = false;
                enabled = true;
                nullable = false;
            }
            column {
                columnCompressionType = ColumnCompressionType.INHERIT;
                fieldStorageType = FieldStorageType.INHERIT;
                mapping = java.sql.Timestamp.class;
                name = "uploaded";
                autoincrement = false;
                enabled = true;
                nullable = false;
            }
            primaryKeyColumn {
                name = "id";
                enabled = true;
            }
            index {
                name = "PRIMARY";
                enabled = true;
                unique = true;
                indexColumn {
                    name = "id";
                    orderType = OrderType.ASC;
                    enabled = true;
                }
            }
            index {
                name = "image_uploader_to_user_id";
                enabled = true;
                unique = false;
                indexColumn {
                    name = "uploader";
                    orderType = OrderType.ASC;
                    enabled = true;
                }
            }
            foreignKey {
                name = "image_uploader_to_user_id";
                enabled = true;
                foreignKeyColumn {
                    foreignColumnName = "id";
                    foreignTableName = "user";
                    name = "uploader";
                    enabled = true;
                }
            }
        }
        table {
            columnCompressionType = ColumnCompressionType.INHERIT;
            fieldStorageType = FieldStorageType.INHERIT;
            name = "link";
            storageEngineType = StorageEngineType.INHERIT;
            tableName = "link";
            enabled = true;
            column {
                columnCompressionType = ColumnCompressionType.INHERIT;
                fieldStorageType = FieldStorageType.INHERIT;
                mapping = java.lang.Long.class;
                name = "follower";
                autoincrement = false;
                enabled = true;
                nullable = false;
            }
            column {
                columnCompressionType = ColumnCompressionType.INHERIT;
                fieldStorageType = FieldStorageType.INHERIT;
                mapping = java.lang.Long.class;
                name = "follows";
                autoincrement = false;
                enabled = true;
                nullable = false;
            }
            primaryKeyColumn {
                name = "follower";
                enabled = true;
            }
            primaryKeyColumn {
                name = "follows";
                enabled = true;
            }
            index {
                name = "PRIMARY";
                enabled = true;
                unique = true;
                indexColumn {
                    name = "follower";
                    orderType = OrderType.ASC;
                    enabled = true;
                }
                indexColumn {
                    name = "follows";
                    orderType = OrderType.ASC;
                    enabled = true;
                }
            }
            index {
                name = "follow_follows_to_user_id";
                enabled = true;
                unique = false;
                indexColumn {
                    name = "follows";
                    orderType = OrderType.ASC;
                    enabled = true;
                }
            }
            foreignKey {
                name = "follow_follower_to_user_id";
                enabled = true;
                foreignKeyColumn {
                    foreignColumnName = "id";
                    foreignTableName = "user";
                    name = "follower";
                    enabled = true;
                }
            }
            foreignKey {
                name = "follow_follows_to_user_id";
                enabled = true;
                foreignKeyColumn {
                    foreignColumnName = "id";
                    foreignTableName = "user";
                    name = "follows";
                    enabled = true;
                }
            }
        }
        table {
            columnCompressionType = ColumnCompressionType.INHERIT;
            fieldStorageType = FieldStorageType.INHERIT;
            name = "user";
            storageEngineType = StorageEngineType.INHERIT;
            tableName = "user";
            enabled = true;
            column {
                columnCompressionType = ColumnCompressionType.INHERIT;
                fieldStorageType = FieldStorageType.INHERIT;
                mapping = java.lang.Long.class;
                name = "id";
                autoincrement = true;
                enabled = true;
                nullable = false;
            }
            column {
                columnCompressionType = ColumnCompressionType.INHERIT;
                fieldStorageType = FieldStorageType.INHERIT;
                mapping = java.lang.String.class;
                name = "mail";
                autoincrement = false;
                enabled = true;
                nullable = false;
            }
            column {
                columnCompressionType = ColumnCompressionType.INHERIT;
                fieldStorageType = FieldStorageType.INHERIT;
                mapping = java.lang.String.class;
                name = "password";
                autoincrement = false;
                enabled = true;
                nullable = false;
            }
            column {
                columnCompressionType = ColumnCompressionType.INHERIT;
                fieldStorageType = FieldStorageType.INHERIT;
                mapping = java.lang.String.class;
                name = "first_name";
                autoincrement = false;
                enabled = true;
                nullable = true;
            }
            column {
                columnCompressionType = ColumnCompressionType.INHERIT;
                fieldStorageType = FieldStorageType.INHERIT;
                mapping = java.lang.String.class;
                name = "last_name";
                autoincrement = false;
                enabled = true;
                nullable = true;
            }
            column {
                columnCompressionType = ColumnCompressionType.INHERIT;
                fieldStorageType = FieldStorageType.INHERIT;
                mapping = java.lang.String.class;
                name = "avatar";
                autoincrement = false;
                enabled = true;
                nullable = true;
            }
            primaryKeyColumn {
                name = "id";
                enabled = true;
            }
            index {
                name = "PRIMARY";
                enabled = true;
                unique = true;
                indexColumn {
                    name = "id";
                    orderType = OrderType.ASC;
                    enabled = true;
                }
            }
        }
    }
}
