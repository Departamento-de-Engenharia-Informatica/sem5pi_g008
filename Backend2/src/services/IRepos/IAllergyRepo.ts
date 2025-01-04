import {Repo} from "../../core/infra/Repo";
import {Allergy} from "../../domain/Allergy/Allergy";
import {MedicalCondition} from "../../domain/MedicalCondition/MedicalCondition";

export default interface IAllergyRepo extends Repo<Allergy> {
  save(allergy: Allergy,  number?: number): Promise<Allergy>;
  getAll(): Promise<Allergy[]>;
  getById(id: string): Promise<Allergy>;
  search(allergy: any): Promise<any>;
  getByDomainId(id: number): Promise<Allergy>;
  updateUsingDomainId(allergy: Allergy, ...fieldsToUpdate: string[]): Promise<Allergy>;

}
