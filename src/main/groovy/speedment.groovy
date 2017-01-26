/**
 *
 * Copyright (c) 2006-2015, Speedment, Inc. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); You may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at:
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */
import java.sql.Timestamp;

name = "socialnetwork";

dbms {
    name = 'db0';
    ipAddress = "speedment.com";
    username = "socialnetwork";
    schema { 
        name = 'socialnetwork';
        table {
            name = "user";
            column {
                name = "id";
                mapping = Long.class;
                nullable = false;
                autoincrement = true;
            }
            column {
                name = "mail";
                mapping = String.class;
                nullable = false;
            }
            column {
                name = "password";
                mapping = String.class;
                nullable = false;
            }
            column {
                name = "first_name";
                mapping = String.class;
            }
            column {
                name = "last_name";
                mapping = String.class;
            }
            column {
                name = "avatar";
                mapping = String.class;
            }
            primaryKeyColumn {
                name = "id"
            }
        }
        table {
            name = "image";
            column {
                name = "id";
                mapping = Long.class;
                nullable = false;
                autoincrement = true;
            }
            column {
                name = "uploader";
                mapping = Long.class;
                nullable = false;
            }
            column {
                name = "title";
                mapping = String.class;
                nullable = false;
            }
            column {
                name = "description";
                mapping = String.class;
                nullable = false;
            }
            column {
                name = "img_data";
                mapping = String.class;
                nullable = false;
            }
            column {
                name = "uploaded";
                mapping = Timestamp.class;
                nullable = false;
            }
            primaryKeyColumn {
                name = "id";
            }
            foreignKey {
                name = "image_uploader_to_user_id";
                foreignKeyColumn {
                    name = "uploader";
                    foreignTableName = "user";
                    foreignColumnName = "id";
                }
            }
        }
        table {
            name = "link";
            column {
                name = "follower";
                mapping = Long.class;
                nullable = false;
            }
            column {
                name = "follows";
                mapping = Long.class;
                nullable = false;
            }
            primaryKeyColumn {
                name = "follower"
            }
            primaryKeyColumn {
                name = "follows"
            }
            foreignKey {
                name = "follow_follower_to_user_id";
                foreignKeyColumn {
                    name = "follower";
                    foreignTableName = "user";
                    foreignColumnName = "id";
                }
            }
            foreignKey {
                name = "follow_follows_to_user_id";
                foreignKeyColumn {
                    name = "follows";
                    foreignTableName = "user";
                    foreignColumnName = "id";
                }
            }
        }
    }
}