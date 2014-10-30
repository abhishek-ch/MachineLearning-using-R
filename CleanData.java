package com.finance;

import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import au.com.bytecode.opencsv.CSVReader;


public class CleanData
{

	public static void main(String[] args)
	{
		
		List<String> list = new ArrayList<String>();
		List<String> sublist = new ArrayList<String>();
		List<String[]> readCSV = readCSV("D:/Work/RWorkSpace/icici.csv");
		for (int i = 0; i < readCSV.size(); i++)
		{
			String[] strings = readCSV.get(i);
			String str = strings[2];
			if(str.contains("ATM/CASH") || str.contains("NFS/CASH") ){
				String[] split = str.split(" ");
				list.add(split[0]); //ATM 
				sublist.add("-");
			}else if(str.contains("BIL")){
				String[] split = str.split("/");
				list.add("PAYMENT");
				sublist.add(split[2]);
			}else if(str.contains("IRCTC")){
				String[] split = str.split(" ");
				list.add(split[0]); //ATM 
				sublist.add("-");
			}else if(str.startsWith("NEFT")){
				list.add("NEFT"); //ATM 
				sublist.add("-");
			}else if(str.contains("SPENCERS")){
				list.add("RETAIL"); //ATM 
				sublist.add("-");
			}else if(str.contains("GANESH INFRATECH") || str.contains("BHOPAL")){
				list.add("BHOPAL"); //ATM 
				sublist.add("-");
			}else{
				String[] split = str.split(" ");
				if(split.length > 0){
					if(split.length == 2){
						list.add(split[0]+"."+split[1]);
					}else{
						list.add(getRefreshed(split[0]));
					}
				}else{
					String[] quote = str.split("/");
					if(quote.length > 0){
						list.add(getRefreshed(quote[0]));
					}else{
						list.add(str);
					}

				}
				sublist.add("-");
			}
		}
		
//		for (String val : list)
//		{
//			System.out.println(val);
//		}
		for (String val : sublist)
		{
			System.out.println(val);
		}
	}
	
	private static String getRefreshed(String output){
		String sub = output;
		if(sub.contains("TATA")){
			sub = "TATA";
		}else if(sub.contains("rent")){
			sub = "RENT";
		}
		
		return sub;
	}
	
	public static List<String[]> readCSV(String fileName){
		try
		{
			CSVReader reader = new CSVReader(new FileReader(fileName), ',');
			List<String[]> readAll = reader.readAll();
			
			return readAll;
		}
		catch (IOException e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}
}
