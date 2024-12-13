import {Inject, Service} from "typedi";
import IAllergyRepo from "../services/IRepos/IAllergyRepo";
import {Document, Model} from "mongoose";
import {IAllergyPersistence} from "../dataschema/IAllergyPersistence";
import {Allergy} from "../domain/Allergy/Allergy";
import {AllergyMap} from "../mappers/AllergyMap";

@Service()
export default class AllergyRepo implements IAllergyRepo {

  constructor(@Inject('allergySchema') private allergySchema: Model<IAllergyPersistence & Document>,) {
  }


  public async save(allergy: Allergy, number?: number): Promise<Allergy> {
    //TODO -  Implementar id autoincrementável
    if(number === undefined) {
      number = await this.getLastId();
    }

    const rawAllergy: any = AllergyMap.toPersistence(allergy, number);
    const allergyCreated = await this.allergySchema.create(rawAllergy);
    return AllergyMap.toDomain(allergyCreated);
  }

  public async exists(allergy: Allergy): Promise<boolean> {
    return true;
  }

  private async getLastId(): Promise<number> {
    //TODO - Implement last id
    return 0;
  }
}
