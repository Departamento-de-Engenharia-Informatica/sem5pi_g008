import {Inject, Service} from "typedi";
import IAllergyRepo from "../services/IRepos/IAllergyRepo";
import {Document, Model} from "mongoose";
import {IAllergyPersistence} from "../dataschema/IAllergyPersistence";
import {Allergy} from "../domain/Allergy/Allergy";
import {AllergyMap} from "../mappers/AllergyMap";
import {AllergyId} from "../domain/Allergy/AllergyId";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";

@Service()
export default class AllergyRepo implements IAllergyRepo {

  constructor(@Inject('allergySchema') private allergySchema: Model<IAllergyPersistence & Document>,) {
  }


  public async save(allergy: Allergy, number?: number): Promise<Allergy> {

    if(number === undefined) {
      number = await this.getFirstAvailableId();
    }

    const rawAllergy: any = AllergyMap.toPersistence(allergy, number);
    const allergyCreated = await this.allergySchema.create(rawAllergy);
    return AllergyMap.toDomain(allergyCreated);
  }

  public async exists(allergy: Allergy): Promise<boolean> {
    try {
      const existingAllergy = await this.allergySchema.findOne({ allergy: allergy.allergy });

      existingAllergy || await this.allergySchema.findOne({ domainId: allergy.domainId.id.toValue() });

      if (existingAllergy) {
        return true;
      }

      return false;

    } catch (error) {
      console.error('Error checking if allergy exists:', error);
      throw error;
    }
  }

  private async getFirstAvailableId(): Promise<number> {
    let number = 1;

    const allergies = await this.allergySchema.find();

    if (allergies.length > 0) {
      number = allergies[allergies.length - 1].domainId + 1;
    }

    return number;
  }

  public async getAll(): Promise<Allergy[]> {
    const allergies = await this.allergySchema.find();

    let aux: Allergy[] = new Array(allergies.length);

    for(let i = 0; i < allergies.length; i++) {
      aux[i] = AllergyMap.toDomain(allergies[i]);
    }

    return aux;
  }

  public async getById(id: string): Promise<Allergy> {
    return this.allergySchema.findOne({_id: id});
  }
}
