import java.util.*;

public class PutItIn {
    private final Map<String, Integer> integerMap;

    public PutItIn(final Map<String, Integer> integerMap) {
        this.integerMap = integerMap;
    }

    public String passInto(int num) {
        String key = String.valueOf(num);
        this.integerMap.put(key, num);

        return key;
    }

    public List<String> passInto(Collection<Integer> integers) {
        List<String> res = new ArrayList<String>();

        for (Integer i : integers) {
            res.add(passInto(i));
        }

        return res;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (String key : this.integerMap.keySet()) {
            sb.append(key + "=" + this.integerMap.get(key));
            sb.append("\n");
        }
        return sb.toString();
    }

    public static void main(String... args) {
        List<Integer> intList = new ArrayList<Integer>();
        List<Integer> otherIntList = new ArrayList<Integer>();

        Integer x = 2;
        Integer y = -1;

        for (int i = 0; i < 2; i++) {
            intList.add(x * i);
            otherIntList.add(x * y * i);
        }

        Map<String, Integer> input = new HashMap<String, Integer>();
        input.put("x", x);
        input.put("y", y);

        PutItIn putItIn = new PutItIn(input);
        System.out.println(putItIn);

        x = intList.get(0);
        y = otherIntList.get(1);
        putItIn.integerMap.put("x", x++);
        putItIn.integerMap.put("y", --y);

        System.out.println(putItIn);

        putItIn.passInto(intList);
        putItIn.passInto(otherIntList);

        putItIn.integerMap.put("0", y);
        x = putItIn.integerMap.get("x");
        x = y;
        putItIn.integerMap.put("x", x);

        System.out.println(putItIn);
        System.out.println(intList.get(0));
        System.out.println(otherIntList.get(1));
    }

}
