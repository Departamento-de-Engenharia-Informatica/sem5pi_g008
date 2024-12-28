import IAllergyDTO from "../../dto/IAllergyDTO";
import {Allergy} from "../../domain/Allergy/Allergy";

export default interface IAllergyService {
  createAllergy(allergy: IAllergyDTO): Promise<any>;
  getAllAllergies(): Promise<Allergy[]>;
  getAllergyFromId(id: string): Promise<Allergy>;
  searchAllergies(allergy: string): Promise<any>;
  updateAllergyDesignation(id: string, designation: string): Promise<any>;
  updateAllergyDescription(id: string, description: string): Promise<any>;
  updateAllergyEffects(id: string, effects: string[]): Promise<any>;

}
