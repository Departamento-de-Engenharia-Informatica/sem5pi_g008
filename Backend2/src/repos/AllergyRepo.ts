import {Inject, Service} from "typedi";
import IAllergyRepo from "../services/IRepos/IAllergyRepo";
import {Document, Model} from "mongoose";
import {IAllergyPersistence} from "../dataschema/IAllergyPersistence";
import {Allergy} from "../domain/Allergy/Allergy";
import {AllergyMap} from "../mappers/AllergyMap";
import {MedicalCondition} from "../domain/MedicalCondition/MedicalCondition";
import {NotFoundException} from "../Exceptions/NotFoundException";
import {MedicalConditionMap} from "../mappers/MedicalConditionMap";


@Service()
export default class AllergyRepo implements IAllergyRepo {

    constructor(@Inject('allergySchema') private allergySchema: Model<IAllergyPersistence & Document>,) {
    }


    public async save(allergy: Allergy, number?: number): Promise<Allergy> {

        if (number === undefined) {
            number = await this.getFirstAvailableId();
            console.log('Number:', number);
        }

        const rawAllergy: any = AllergyMap.toPersistence(allergy, number);
        const allergyCreated = await this.allergySchema.create(rawAllergy);
        return AllergyMap.toDomain(allergyCreated);
    }

    public async exists(allergy: Allergy): Promise<boolean> {
        try {
            const existingAllergy = await this.allergySchema.findOne({allergy: allergy.designation});

            existingAllergy || await this.allergySchema.findOne({domainId: allergy.domainId.id.toValue()});

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

        for (let i = 0; i < allergies.length; i++) {
            aux[i] = AllergyMap.toDomain(allergies[i]);
        }

        return aux;
    }


  public async getById(id: string): Promise<Allergy> {
    return this.allergySchema.findOne({_id: id});
  }

    public async search(allergy: string): Promise<Allergy> {

        const allergies = await this.allergySchema.findOne({allergy: allergy});
        return AllergyMap.toDomain(allergies);
    }

    public async getByDomainId(id: number): Promise<Allergy> {
        const allergy = await this.allergySchema.findOne({domainId: id});

        if (!allergy) {
            throw new NotFoundException("Allergy not found");
        }

        return AllergyMap.toDomain(allergy);
    }

    public async updateUsingDomainId(allergy: Allergy, ...fieldsToUpdate: string[]): Promise<Allergy> {
        const domainId = allergy.domainId.value;
        const rawAllergy = AllergyMap.toPersistence(allergy, domainId);

        delete rawAllergy.code;
        delete rawAllergy.domainId;
        delete rawAllergy.isDeleted;

        const remainingFields = ['description', 'designation', 'effects'];

        for (const field of fieldsToUpdate) {
            if (!remainingFields.includes(field)) {
                throw new Error("Invalid field to update");
            }
        }

        Object.keys(rawAllergy).forEach((key) => {
            if (!fieldsToUpdate.includes(key)) {
                delete rawAllergy[key];
            }
        });

        const updatedAllergy = await this.allergySchema.findOneAndUpdate(
            {domainId: domainId},
            rawAllergy,
            {new: true}
        );

        if (!updatedAllergy) {
            throw new NotFoundException("Allergy not found");
        }

        return AllergyMap.toDomain(updatedAllergy);
    }
    
}
