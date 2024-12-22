import {Repo} from "../../core/infra/Repo";
import {Allergy} from "../../domain/Allergy/Allergy";

export default interface IAllergyRepo extends Repo<Allergy> {
  save(allergy: Allergy,  number?: number): Promise<Allergy>;
  getAll(): Promise<Allergy[]>;
  getById(id: string): Promise<Allergy>;
}
