import IAllergyDTO from "../../dto/IAllergyDTO";
import {Allergy} from "../../domain/Allergy/Allergy";

export default interface IAllergyService {
  createAllergy(allergy: IAllergyDTO): Promise<any>;
  getAllAllergies(): Promise<Allergy[]>;
  searchAllergies(allergy: string): Promise<any>;}

