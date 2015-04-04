import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
//

public class MultiConstructor {
    List<String> stringList = new ArrayList<String>();

    public MultiConstructor(List<String> stringList) {
        System.out.println("call with stringlist");
        this.stringList = stringList;
    }

    public MultiConstructor(List<Integer> integerList) {
        System.out.println("call with intlist");
        for (Integer i : integerList) {
            this.stringList.add(i == null ? "null" : i.toString());
        }
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (String str : stringList) {
            sb.append(str);
            sb.append("\n");
        }
        return sb.toString();
    }

    public static void main(String[] args) {
        List<String> strList = Arrays.asList("1", "2", "3");
        List<Integer> intList = Arrays.asList(1, 2, 3, null);
        //

        System.out.println(new MultiConstructor(strList));
        System.out.println(new MultiConstructor(intList));
        //
    }
}
