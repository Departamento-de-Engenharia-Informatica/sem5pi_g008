import {Inject, Service} from "typedi";
import {Document, Model} from "mongoose";
import IMedicalRecordFreeTextRepo from "../services/IRepos/IMedicalRecordFreeTextRepo";
import {MedicalRecordFreeText} from "../domain/MedicalRecordFreeText/MedicalRecordFreeText";
import {MedicalRecordFreeTextMap} from "../mappers/MedicalRecordFreeTextMapper";
import {IMedicalRecordFreeTextPersistence} from "../dataschema/IMedicalRecordFreeTextPersistence";
import {MedicalRecordAllergy} from "../domain/MedicalRecordAllergy/MedicalRecordAllergy";
import {MedicalRecordAllergyMapper} from "../mappers/MedicalRecordAllergyMapper";

@Service()
export default class MedicalRecordFreeTextRepo implements IMedicalRecordFreeTextRepo{
  constructor(@Inject('medicalRecordFreeTextSchema') private medicalRecordFreeTextSchema: Model<IMedicalRecordFreeTextPersistence & Document>,) {
  }

  exists(t: MedicalRecordFreeText): Promise<boolean> {
    //TODO:Implement exists method
    return Promise.resolve(false);
  }

  public async save(medicalRecordFreeText: MedicalRecordFreeText): Promise<MedicalRecordFreeText> {

    const id = await this.getLastId();

    const rawMedicalRecordFreeText: any = MedicalRecordFreeTextMap.toPersistence(medicalRecordFreeText, id);

    const medicalRecordFreeTextCreated = await this.medicalRecordFreeTextSchema.create(rawMedicalRecordFreeText);

    return MedicalRecordFreeTextMap.toDomain(medicalRecordFreeTextCreated);
  }


  public async getLastId(): Promise<number> {

    let number = 1;

    const medicalRecord = await this.medicalRecordFreeTextSchema.find();

    if (medicalRecord.length > 0) {
      number = medicalRecord.length + 1;
    }

    return number;
  }

  public async getByMedicalId(medicalRecordId: string): Promise<MedicalRecordFreeText[]> {
    const medicalRecordFreeTexts = await this.medicalRecordFreeTextSchema
        .find({medicalRecordId: medicalRecordId})
        .exec();
  
    return medicalRecordFreeTexts.map(MedicalRecordFreeTextMap.toDomain);
  }
  
}
