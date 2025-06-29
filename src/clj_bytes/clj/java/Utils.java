package clj_bytes.clj.java;

import java.util.Arrays;

public class Utils {
    public static int indexOfBytes(byte[] bytes, byte[] sep) {
        return indexOfBytes(bytes, 0, bytes.length, sep);
    }

    public static int indexOfBytes(byte[] bytes, int fromIndex, int toIndex, byte[] sep) {
        int sepLength = sep.length;
        int endSearchIndex = toIndex - sepLength;
        for (int i = fromIndex; i <= endSearchIndex; i++) {
            if (Arrays.equals(bytes, i, i + sepLength, sep, 0, sepLength)) {
                return i;
            }
        }
        return -1;
    }

    public static int bytesArrayLength(byte[][] bytesArray) {
        int length = 0;
        for (int i = 0; i < bytesArray.length; i++) {
            length += bytesArray[i].length;
        }
        return length;
    }

    public static byte[] joinBytesArray(byte[][] bytesArray) {
        byte[] bytes = new byte[bytesArrayLength(bytesArray)];
        int offset = 0;
        for (int i = 0; i < bytesArray.length; i++) {
            byte[] newBytes = bytesArray[i];
            System.arraycopy(newBytes, 0, bytes, offset, newBytes.length);
            offset += newBytes.length;
        }
        return bytes;
    }
}
