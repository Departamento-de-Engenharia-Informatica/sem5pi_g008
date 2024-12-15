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

    if(number === undefined) {
      number = await this.getFirstAvailableId();
      console.log('Number:', number);
    }

    const rawAllergy: any = AllergyMap.toPersistence(allergy, number);
    const allergyCreated = await this.allergySchema.create(rawAllergy);
    return AllergyMap.toDomain(allergyCreated);
  }

  public async exists(allergy: Allergy): Promise<boolean> {
    return true;
  }

  private async getFirstAvailableId(): Promise<number> {
    try {

      const existingIds = await this.allergySchema.find().sort({ domainId: 1 }).select('domainId');

      if (existingIds.length === 0) {
        return 0;
      }

      for (let i = 0; i < existingIds.length; i++) {
        if (existingIds[i].domainId !== i) {
          return i;
        }
      }

      return existingIds[existingIds.length - 1].domainId + 1;
    } catch (error) {
      console.error('Error fetching first available ID:', error);
      throw error;
    }
  }


}
