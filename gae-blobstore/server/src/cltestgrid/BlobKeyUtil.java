/* Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
 * See LICENSE for details.
 */
package cltestgrid;

class BlobKeyUtil {
  public static boolean isCloudStorageKey(String str)  {
    return str.length() == 10;
  }
}
